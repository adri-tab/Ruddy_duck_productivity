# Loading packages -------------------------------------------------------------------

Sys.setlocale("LC_ALL", "English")

require(tidyverse)
require(rlang)
require(xlsx)
require(readxl)
require(lubridate)
require(nimble)
require(MCMCvis)
require(mcmcplots)
require(tls) # total least squares regression

# Colours selection -------------------------------------------------------------------

tibble(id = 1:20) %>% 
  ggplot(aes(x = 1, y = id, color = factor(id))) +
  geom_point(size = 10) + scale_y_continuous(aes(breaks = id)) -> p; p

ggplot_build(p)$data

c_pop <- c("#F8766D", "#619CFF")
c_met <- c("#00BF7D", "#D89000")

# Plot functions --------------------------------------------------------------------

scale_y_percent <- list(
  scale_y_continuous(
  labels = scales::percent_format(accuracy = 1L),
  limits = c(0, 1),
  breaks = 0:10 / 10))

scale_x_date_own <- function(coef = 5e-2) {
  list(
    scale_x_date(date_breaks = "2 years", 
                 date_minor_breaks = "1 year", 
                 date_labels = "%Y", 
                 expand = c(coef, coef)),
    guides(x =  guide_axis(angle = 45)),
    labs(x = NULL) 
  )}

raw_plot <- function(para,
                     title = NA, 
                     ylimits = NA, 
                     percent = FALSE) {
  para = enquo(para)
  
  JuvOut4 %>%
    filter(par == as_name(para)) %>% 
    ggplot(aes(x = year, y = `50%`, color = method, fill = method)) +
    facet_wrap(~ pop, nrow = 1, scales = "free_x") +
    geom_line(linetype = "dashed", alpha = .5) +
    geom_point() +
    geom_linerange(aes(ymin = `2.5%`, ymax = `97.5%`), alpha = .5) +
    scale_y_continuous(limits = if (is.na(ylimits)) {c(0, NA)} else {ylimits}) +
    scale_x_date(date_breaks = "2 years", 
                 date_minor_breaks = "1 year", 
                 date_labels = "%Y") +
    guides(x =  guide_axis(angle = 45)) +
    scale_color_manual(values = c_met) +
    labs(title = title, x = NULL, y = NULL) -> tmp
  
  if (percent == TRUE) {tmp + scale_y_percent} else {tmp}
}

cor_plot <- function(col, title = NULL, limits = NA) {
  
  col2 <- str_c(col, c("2.5%", "50%", "97.5%"), sep = "_")
  col_s <- str_c(col2, "sampling", sep = "_")
  col_c <- str_c(col2, "counting", sep = "_")
  
  tmp <- JuvOut4 %>%
    filter(par == col) %>%
    add_count(pop, year) %>% 
    filter(n == 2) %>% 
    pivot_longer(contains("%")) %>% 
    mutate(par = str_c(par, name, method, sep = "_"), 
           year = year(year) %>% as.character()) %>% 
    select(year, par, value) %>% 
    pivot_wider(names_from = par, values_from = value)
  
  tls_data <- tmp  %>% 
    rename(cnt = !!sym(col_c[2]), spl = !!sym(col_s[2])) %>% 
    select(cnt, spl)
  
  slope <- c(tls = tls(cnt ~ 0 + spl, tls_data)$coefficient,
             ols =  lm(rlang::expr(!!sym(col_c[2]) ~ 0 + !!sym(col_s[2])), 
                       tmp)$coef)
    
  tmp %>%   
    ggplot(aes(x = !!sym(col_s[2]), y = !!sym(col_c[2]))) +
    geom_abline(intercept = 0, 
                slope = slope[1],
                color = "gray", size = 1.3) +
    geom_abline(slope = 1, linetype = "dashed") +
    geom_point(aes(color = year)) +
    geom_errorbarh(aes(xmin = !!sym(col_s[1]), xmax = !!sym(col_s[3]), color = year), 
                   height = 0) +
    geom_errorbar(aes(ymin = !!sym(col_c[1]), ymax = !!sym(col_c[3]), color = year), 
                  width = 0) +
    labs(title = title, 
         y = "counting method", x = "sampling method") +
    scale_x_continuous(expand = expansion(mult = c(0, 0)), 
                       limits = if (is.na(limits)) {
                         c(0, 1.1 * max(unlist(tmp[-1])))
                       } else {limits}) +
    scale_y_continuous(expand = expansion(mult = c(0, 0)), 
                       limits = if (is.na(limits)) {
                         c(0, 1.1 * max(unlist(tmp[-1])))
                       } else {limits})
}

# Data import -----------------------------------------------------------------------

read_rds("../Ruddy_duck_data/Output/Ruddy_duck_data.rds") %>% pluck(1) %>% 
  mutate(across(pop, as_factor)) %>% arrange(pop) -> frag

frag %>% 
  filter(age == 'ad', sex != "ind") %>% 
  group_by(year, pop, sex) %>% 
  summarize(across(shot, sum)) %>% 
  group_by(year, pop) %>% 
  mutate(tot = sum(shot)) %>%  
  filter(sex == "mal", tot > 0) %>% 
  group_by(pop) %>% 
  mutate(inf = qbeta(0.025, shot, tot - shot), 
         male_proportion = qbeta(0.5, shot, tot - shot), 
         sup = qbeta(0.975, shot, tot - shot),
         avg_inf = qbeta(0.025, sum(shot), sum(tot - shot)),
         avg = qbeta(0.5, sum(shot), sum(tot - shot)),
         avg_sup = qbeta(0.975, sum(shot), sum(tot - shot))) %>% 
  ggplot(aes(x = year, y = male_proportion, color = pop)) +
  geom_ribbon(aes(ymin = avg_inf, ymax = avg_sup), alpha = 0.3, color = "gray") +
  geom_hline(aes(yintercept = avg), linetype = "dashed") +
  geom_linerange(aes(ymin = inf, ymax = sup), alpha = .5) +
  geom_point(aes(size = tot), alpha = .8) +
  facet_wrap( ~ pop, ncol = 2) +
  scale_color_manual(values = c_pop, guide = "none") +
  scale_y_percent +
  scale_x_date_own(5e-2) +
  guides(size = guide_legend(title = "Samples")) +
  labs(y = "Male proportion")

read_rds("../Ruddy_duck_data/Output/Ruddy_duck_data.rds") %>% pluck(2) %>% 
  mutate(across(pop, as_factor)) %>% arrange(pop) -> counts

# Count and sample formatting ------------------------------------------------------

# one keeps only counts from Dec and Jan
# one keeps only samples when total > 100
counts %>% 
  unnest(age) %>% 
  unnest(sex_app) %>% 
  filter(obs_type_mal + obs_type_fem > 0, month(date) %in% c(12, 1)) %>% 
  group_by(across(c(year:no_ad, pop))) %>% 
  summarize(across(starts_with("obs"), sum)) %>% 
  ungroup() %>% 
  filter(obs_type_mal + obs_type_fem > 100) %>% 
  mutate(across(c(ad, no_ad), ~ if_else(ad + no_ad > 100, .x, NA_real_))) %>% 
  left_join(
    counts %>% 
      mutate(year = year + years(1),
             n_breed = count - killed_before_rep) %>% 
      select(year, pop, n_breed = count)) %>% 
  full_join(counts %>% select(year, count, pop)) %>% 
  rename(n_pop = count) %>% 
  arrange(pop, year) -> counts_1

# male and female proportion in samples
# one keeps only years with only few unidentified sex
# when regulation starts and prevents from estimating r_max?

frag %>% 
  group_by(year, pop, age, sex) %>% 
  summarise(across(shot, sum)) %>% 
  ggplot(aes(x = year, y = shot, fill = interaction(sex, age))) +
  geom_col() +
  facet_wrap( ~ pop, ncol = 1, scales = "free_y") + 
  scale_x_date_own(0)

frag %>%  
  filter(age == "ad", sex != "ind") %>% 
  filter(!(pop == "FR" & year(year) < 2011), !(pop == "UK" & year(year) < 2002)) %>%
  group_by(sex) %>% 
  summarize(across(shot, sum)) %>% 
  pivot_wider(names_from = sex, values_from = shot) %>% 
  select(kill_f = fem, kill_m = mal) %>% 
  bind_cols(counts_1) %>% 
  relocate(c(n_pop, kill_m, kill_f), .after = n_breed) %>% 
  mutate(obs_tot = obs_type_mal + obs_type_fem,
         spl_tot = ad + no_ad) %>% 
  rowid_to_column() -> ds_1

# Lambda max formatting ------------------------------------------------------------

# estimation of lambda_max to test productivity reliability
# when ruddy ducks start to be permanently seen?
counts_1 %>% 
  ggplot(aes(x = year, y = n_pop, color = pop)) + 
  geom_line() +
  geom_point() +
  facet_wrap(~ pop, ncol = 1) +
  theme(legend.position = "none") +
  scale_y_log10() +
  scale_x_date_own(1e-2)

# from 1961 in UK, from 1994 in FR
# growth shift from 1981 in UK

# when exploitation starts and prevents from estimating lambda_max
frag %>% 
  group_by(year, pop, age, sex) %>% 
  summarise(across(shot, sum)) %>% 
  ggplot(aes(x = year, y = shot, fill = interaction(sex, age))) +
  geom_col() +
  facet_wrap( ~ pop, ncol = 1, scales = "free_y") +
  scale_x_date_own(0)

counts_1 %>% 
  left_join(frag %>% 
              filter(age == "ad") %>% 
              group_by(year, pop) %>% 
              summarize(across(shot, sum)) %>% 
              ungroup() %>% 
              filter(shot > 0)) %>%
  mutate(across(shot, replace_na, 0),
         exploitation_rate = (shot / n_pop) %>% replace_na(0)) %>% 
  ggplot(aes(x = year, y = exploitation_rate, color = pop)) +
  geom_hline(yintercept = c(.1, .2), linetype = "dashed") +
  geom_point() +
  geom_line() +
  facet_wrap( ~ pop, nrow = 2) +
  scale_y_percent +
  scale_x_date_own(1e-2) +
  scale_color_manual(values = c_pop)
# To 1998 in UK, and to 1995 in FR si 0%
# To 1998 in UK, and to 2003 in FR si 10%
# To 2005 in UK, and to 2007 in FR si 20%

counts_1 %>% 
  filter(!(pop == "UK" & year(year) > 1998), !(pop == "UK" & year(year) < 1961),
         !(pop == "FR" & year(year) > 2003), !(pop == "FR" & year(year) < 1994)) %>% 
  mutate(size_pop = case_when(
    pop == "FR" ~ "small pop.",
    pop == "UK" & year(year) < 1981 ~ "small pop.",
    pop == "UK" & year(year) > 1980 ~ "large pop.",
    TRUE ~ NA_character_),
    sub_pop = str_c(pop, " ", size_pop) %>% as_factor()) %>% 
  group_by(sub_pop) %>% 
  mutate(explo = max(year) + years(1))  %>% 
  group_by(pop) %>% 
  mutate(explo = max(explo)) %>% 
  ungroup() %>% 
  select(year, n_pop, pop, size_pop, sub_pop, explo) %>% 
  arrange(sub_pop, year) -> counts_2

# lambda
counts_2 %>% 
  mutate(log_N = log(n_pop)) %>% 
  group_by(sub_pop) %>% 
  nest() %>% 
  mutate(data = data %>% 
           map(~ .x %>% 
                 mutate(log_N_prev = c(NA_real_, .x$log_N[-nrow(.x)])))) %>% 
  unnest(data) %>% 
  ungroup() %>% 
  mutate(lambda = log_N - log_N_prev) %>%
  group_by(sub_pop) %>% 
  mutate(avg = mean(lambda, na.rm = TRUE)) %>%
  ggplot(aes(x = year, y = lambda, group = sub_pop, color = pop, shape = sub_pop, 
             alpha = sub_pop)) + 
  facet_wrap( ~ pop, ncol = 1) +
  geom_line(stat = "smooth", method = "lm", formula = y ~ 1) +
  geom_point() +
  scale_x_date_own(1e-2) +
  scale_shape_manual(values = c(16, 16, 16), guide = "none") +
  scale_alpha_manual(values = c(0.5, 1, 1), guide = "none") +
  scale_color_manual(values = c_pop, guide = "none")

# r_max
counts_1 %>% 
  ggplot(aes(x = year, y = n_pop)) + 
  facet_wrap( ~ pop, ncol = 1) +
  geom_vline(data = counts_2, aes(xintercept = explo), linetype = "dashed", 
             color = "#de2d26") +
  geom_line(alpha = 0.5, linetype = "dashed") +
  geom_point(shape = 16, color = "gray") +
  scale_y_log10(minor_breaks = NULL) +
  annotation_logticks(side = "l", color = "grey") +
  geom_text(data = counts_2, label = "control\nstart", 
            aes(x = explo + years(1)), y = 0.5, hjust = 0, 
            size = 3, color = "#de2d26") +
  geom_line(stat = "smooth", data = counts_2, 
            aes(x = year, y = n_pop, group = sub_pop, color = pop, alpha = sub_pop),
            method = "lm") +
  geom_point(data = counts_2, 
             aes(x = year, y = n_pop, group = sub_pop, color = pop, shape = sub_pop, 
                 alpha = sub_pop)) +
 scale_x_date_own(1e-2) +
  scale_shape_manual(values = c(16, 16, 16), guide = "none") +
  scale_alpha_manual(values = c(0.5, 1, 1), guide = "none") +
  scale_color_manual(values = c_pop, guide = "none") +
  labs(y = "Population size") -> raw_pop; raw_pop

# Combined dataset ------------------------------------------------------------------

ds_1 %>% 
  left_join(counts_2 %>% select(year, pop, sub_pop)) %>% 
  mutate(pop_title = case_when(
    pop == "FR" ~ "FR small pop.",
    pop == "UK" & year(year) < 1981 ~ "UK small pop.",
    pop == "UK" & year(year) > 1980 ~ "UK large pop.",
    TRUE ~ NA_character_) %>% as_factor()) -> ds

# Data formatting function ----------------------------------------------------------

dis <- function(fil = NA, gp = NA, col = NA, size = FALSE) {
  fil = enquo(fil)
  gp = enquo(gp)
  col = enquo(col)
  x <- ds %>% 
    filter(!is.na(!!fil)) %>% 
    mutate(id = as.numeric(!!gp)) %>% 
    pull(!!col)
  
  if (size == TRUE) {
    length(x)
  } else {
     x
  }
}

# Model -----------------------------------------------------------------------------

JuvCode <- nimbleCode(
  {
    # conjugate posterior for the male proportion (adult only)
    p_mal ~ dbeta(mal_ad + 1, fem_ad + 1)
    
    # parameter estimation from apparent population structure
    
    #hierarchical prior
    for (i in 1:(C_id_max + S_id_max)) {
      shape1[i] ~ dunif(1, 1e3)
      shape2[i] ~ dunif(1, 1e3)
      
      prior[i] ~ dbeta(shape1[i], shape2[i])
    }
    
    for (j in 1:C) {
      
      #p_mal_ad prior
      # p_mal_ad[j] ~  dbeta(shape1[C_id[j]], shape2[C_id[j]])
      p_mal_ad[j] ~  dbeta(1, 1)
      
      # LL
      cnt_mal_ad[j] ~ dbin(p_mal_ad[j], cnt_tot[j])
      
      p_juv[j] <- (1 - p_mal_ad[j] / p_mal) # * 1.741913
      
      # recruitment and productivity estimation
      recruits[j] <- p_juv[j] * cnt_size_pop[j]
      
      productivity[j] <- recruits[j] / cnt_size_breeding_pop[j]
      
    }
    
    # parameter estimation from hunting samples
    
    for (k in 1:S) {
      
      #p_juv prior
      # p_juv[C + k] ~ dbeta(shape1[C_id_max + S_id[k]], shape2[C_id_max + S_id[k]])
      p_juv[C + k] ~ dbeta(1, 1)
      
      #LL
      spl_juv[k] ~ dbin(p_juv[C + k], spl_tot[k])
      
      # recruitment and productivity estimation
      recruits[C + k] <- p_juv[C + k] * spl_size_pop[k]
      
      productivity[C + k] <- recruits[C + k] / spl_size_breeding_pop[k]
      
    }
    
    # rmax estimation frow counts
    
    # priors
    for (r in 1:R_id_max) {
      
      log_N_0[r] ~ dnorm(0, sd = 1e2)
      r_max[r] ~ dnorm(0, sd = 1e2)
      N_sd[r] ~ dunif(0, 1e3)
      log(lambda_max[r]) <- r_max[r]
      
    }
    
    # LL
    for (t in 1:R) {
      
      rmax_size_pop[t] ~ dlnorm(log_N_0[R_id[t]] + r_max[R_id[t]] * year[t], 
                                sd = N_sd[R_id[t]])
    }
  
    # survival
    for (q in 1:W) {
      survival[q] <- lambda_max[W_id[q] + 1] - productivity[q]
    }
    
  })

# set seed
myseed <- 88
set.seed(myseed)


# model data
JuvConst <- list(C = dis(obs_tot, pop, id, TRUE),
                 C_id = dis(obs_tot, pop, id),
                 C_id_max = dis(obs_tot, pop, id) %>% max(),
                 S = dis(spl_tot, pop, id, TRUE),
                 S_id = dis(spl_tot, pop, id),
                 S_id_max = dis(spl_tot, pop, id) %>% max(),
                 W = dis(obs_tot, pop, id, TRUE) +  dis(spl_tot, pop, id, TRUE),
                 W_id = c(dis(obs_tot, pop, id), dis(spl_tot, pop, id)),
                 R = dis(sub_pop, sub_pop, id, TRUE),
                 R_id = dis(sub_pop, sub_pop, id),
                 R_id_max = dis(sub_pop, sub_pop, id) %>% max())

JuvData <- list(mal_ad = unique(ds$kill_m),
                fem_ad = unique(ds$kill_f),
                cnt_mal_ad = dis(obs_tot, pop, obs_type_mal),
                cnt_tot = dis(obs_tot, pop, obs_tot),
                cnt_size_pop = dis(obs_tot, pop, n_pop),
                cnt_size_breeding_pop = dis(obs_tot, pop, n_breed),
                spl_juv = round(dis(spl_tot, pop, no_ad)),
                spl_tot = dis(spl_tot, pop, spl_tot),
                spl_size_pop = dis(ad, pop, n_pop),
                spl_size_breeding_pop = dis(ad, pop, n_breed),
                rmax_size_pop = dis(sub_pop, sub_pop, n_pop),
                year = ds %>% 
                  filter(!is.na(sub_pop)) %>% 
                  mutate(id = 1) %>% 
                  group_by(sub_pop) %>% 
                  mutate(id = cumsum(id)) %>% 
                  pull(id))

JuvInit <- list(p_mal = 0.6,
                p_mal_ad = rep(.1, JuvConst$C),
                shape1 = rep(1, 3),
                shape2 = rep(1, 3),
                prior = rep(.1, 3),
                p_juv = rep(.1, JuvConst$C + JuvConst$S),
                log_N_0 = rep(0, 3),
                r_max = rep(1, 3),
                N_sd = rep(1, 3))

# node targets
JuvMon <- c("p_mal",
            "prior",
            "p_juv",
            "recruits",
            "productivity",
            "lambda_max",
            "survival")

# mcmc parameters
nsample_1 <- 5e4 ; thin_1 <- 10 ; nburnin_1 <- 2e3 ; nchain_1 <- 1
nsample_1 * thin_1 + nburnin_1

nimbleMCMC(
  code = JuvCode, 
  constants = JuvConst, 
  data = JuvData, 
  inits = JuvInit, 
  monitors = JuvMon,
  niter = nsample_1 * thin_1 + nburnin_1, 
  nburnin = nburnin_1, 
  thin = thin_1, 
  nchains = nchain_1, 
  setSeed = myseed) -> JuvOut

# Output check ----------------------------------------------------------------------

# autocorrelation and burning check
mcmcplot(mcmcout = JuvOut,
         parms = JuvMon[6]) 

# convergence check + vizualisation of prior update
MCMCtrace(object = JuvOut, 
          params = JuvMon[6], 
          iter = nsample_1,
          # priors = runif(nchain_1 * nsample_1, 0, 1), 
          pdf = FALSE,
          ind = TRUE,
          Rhat = TRUE)

# prod, rmax, survival plot
MCMCplot(JuvOut, 
         params = JuvMon[6],
         horiz = FALSE, 
         ylim = c(0, 1.5))

# Output formatting -----------------------------------------------------------------

MCMCsummary(object = JuvOut, 
            Rhat = TRUE, 
            n.eff = TRUE)


MCMCsummary(object = JuvOut, 
            Rhat = FALSE, 
            n.eff = FALSE) -> JuvOut2

# tibble for outputs
JuvOut2 %>%
  as_tibble(rownames = "var") %>%
  # janitor::clean_names() %>% 
  as_tibble() %>% 
  mutate(id = var %>% str_extract("\\d+") %>% as.numeric(),
         par = var %>% str_extract(".*(?=\\[)"),
         par = if_else(is.na(par), var, par),
         par = as_factor(par) %>% fct_relevel(JuvMon),
         rowid = case_when(
           par %in% 
             c("p_juv", 
               "recruits", 
               "productivity", 
               "survival") ~ c(dis(obs_tot, pop, rowid), dis(spl_tot, pop, rowid))[id],
           TRUE ~ NA_integer_)) %>% 
  arrange(par, id) %>% 
  mutate(method = case_when(
    par %in% 
      c("p_juv", 
        "recruits", 
        "productivity", 
        "survival") ~ c(rep("counting", dis(obs_tot, pop, id, TRUE)), 
                        rep("sampling", dis(spl_tot, pop, id, TRUE)))[id],
    par == "lambda_max" ~ levels(dis(sub_pop, sub_pop, sub_pop))[id],
    TRUE ~ NA_character_)) %>% 
  left_join(ds %>% select(rowid, year, pop, sub_pop, pop_title)) %>% 
  select(year, pop, sub_pop, pop_title, par, method, `2.5%`, `50%`, `97.5%`) %>% 
  mutate(across(c(pop, sub_pop), as.character)) %>% 
  mutate(sub_pop = if_else(par == "lambda_max", method, sub_pop),
         pop = if_else(par == "lambda_max", str_trunc(method, 2, "right", ellipsis = ""), 
                       pop),
         method = if_else(par == "lambda_max", NA_character_, method)) %>% 
  mutate(pop = as_factor(pop) %>% fct_relevel(levels(ds$pop)),
         sub_pop = as_factor(sub_pop) %>% fct_relevel(levels(ds$sub_pop)),
         method = as_factor(method)) -> JuvOut3

# average survival
JuvOut3 %>% 
  filter(par == "productivity") %>% 
  rowid_to_column("id") %>% 
  filter(!(year(year) > 2008 & pop == "FR")) %>% 
  mutate(name = str_c(par, "[", id, "]"), 
         gp = str_c(pop, "_", method),
         gth = if_else(gp == "FR_counting", 
                       "lambda_max[3]", 
                       "lambda_max[2]")) -> survival_sel

# survival estimation 
survival_sel %>% 
  split(.$gp) %>% 
  map(function(x) {
    # prod = as_tibble(JuvOut) %>%
    #   select(x %>% pull(name)) %>%
    #   unlist() %>% sample(size = nrow(JuvOut))
    prod = as_tibble(JuvOut) %>%
      select(x %>% pull(name)) %>%
      rowMeans()
    lambda = as_tibble(JuvOut) %>% pull(unique(x$gth))
    survival = quantile(lambda - prod, c(.025, .5, .975))
    out = x %>% 
      distinct(pop, method) %>%
      mutate(sub_pop = if_else(unique(x$pop) == "FR", 
                               "FR small pop.", "UK large pop.")) %>% 
      bind_cols(tibble(survival, name = names(survival)) %>% 
                  pivot_wider(names_from = name, values_from = survival)) %>% 
      mutate(par = "survival_avg")
    return(out)
  }) %>% 
  bind_rows() %>% 
  mutate(sub_pop = as_factor(sub_pop)) -> survival_avg

bind_rows(JuvOut3, survival_avg) -> JuvOut4

# Output plot -----------------------------------------------------------------------

raw_plot(p_juv, title = "Proportion of recruits in the population", 
         percent = TRUE) -> raw_prop; raw_prop
raw_plot(productivity, title = "Productivity: recruits per breeder") + 
  scale_y_continuous(breaks = 0:9 / 10) -> raw_prod; raw_prod
raw_plot(recruits, title = "Number of recruits") + 
  facet_wrap(~ pop_title, nrow = 1, scales = "free") -> raw_recruit; raw_recruit

JuvOut4 %>% 
  filter(!is.na(method), par == "productivity") %>% 
  select(year, pop, method, `50%`) %>% 
  pivot_wider(names_from = method, values_from = `50%`) %>% 
  filter(!is.na(sampling)) -> tls_ds

xx <- tls(counting ~ 0 + sampling, data = tls_ds)$coefficient


cor_plot(col = "p_juv", 
         title = "Proportion of recruits in the population", 
         limits = c(0, 1)) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1L),
    limits = c(0, 1),
    breaks = 0:10 / 10, 
    expand = c(0, 0)) +  
  scale_x_continuous(
      labels = scales::percent_format(accuracy = 1L),
      limits = c(0, 1),
      breaks = 0:10 / 10, 
      expand = c(0, 0)) +  
  guides(x =  guide_axis(angle = 45)) -> cor_prop; cor_prop

cor_plot(col = "productivity", 
         title = "Productivity: recruits per breeder") +
  geom_segment(x = .375, y = .375, xend = .375, yend = xx2 * .375,
               arrow = arrow(type = "closed", length = unit(0.3, "cm")),
               size = .5, color = "#de2d26") +
  geom_text(x = .4, y = .42, 
            label = "1:1", 
            color = "black",
            hjust = 1) +
  geom_text(x = .375, y = .30, 
            label = str_c(" - ", round(1e2 * (1 - xx2), 1), "% "), 
            color = "#de2d26",
            hjust = 0) -> cor_prod; cor_prod
cor_plot(col = "recruits", title = "Recruits") -> cor_recr; cor_recr

raw_plot(survival, title = "Survival") + 
  geom_hline(yintercept = 1, linetype = "dashed")

raw_pop

JuvOut4 %>%
  filter(par == "lambda_max") %>% 
  ggplot(aes(x = sub_pop, y = `50%` - 1, group = sub_pop, color = pop, alpha = sub_pop)) +
  geom_line(linetype = "dashed", alpha = .5) +
  geom_point() +
  geom_linerange(aes(ymin = `2.5%` - 1, ymax = `97.5%` - 1)) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1L),
    limits = c(0, .5),
    breaks = 0:10 / 10) +
  scale_alpha_manual(values = c(0.5, 1, 1), guide = "none") +
  scale_color_manual(values = c_pop, guide = "none") +
  labs(title = bquote('Annual population growth rate (' * lambda * ')'),
       y = NULL, x = NULL) -> lambda_plot; lambda_plot

JuvOut4 %>%
  filter(par == "survival_avg") %>% 
  bind_rows(tibble(pop = "FR", sub_pop = "FR small pop.", 
                   par = "survival_avg", method = "sampling")) %>% 
  mutate(txt = if_else(pop == "FR" & method == "sampling", "NO DATA", NA_character_),
         sub_pop = as_factor(sub_pop) %>% 
           fct_relevel("UK large pop.", "FR small pop.")) %>% 
  ggplot(aes(x = method, y = `50%`, group = sub_pop, color = method)) +
  facet_wrap(~ sub_pop, nrow = 1, scales = "free_x") +
  geom_point() +
  geom_linerange(aes(ymin = `2.5%`, ymax = `97.5%`)) +
  geom_text(aes(x = method, y = .5, label = txt), color = "#de2d26", angle = -90) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1L),
    limits = c(0, 1),
    breaks = 0:10 / 10) +
  scale_color_manual(values = c_met, guide = "none") +
labs(title = "Annual adult survival rate",
       y = NULL, x = NULL) -> surviv_plot; surviv_plot 

