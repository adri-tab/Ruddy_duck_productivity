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
require(scales)
require(gridExtra)
require(cowplot)

theme_set(theme_bw())

# Colours selection -------------------------------------------------------------------

tibble(id = 1:20) %>% 
  ggplot(aes(x = 1, y = id, color = factor(id))) +
  geom_point(size = 10) + 
  scale_y_continuous(aes(breaks = id)) -> p; p

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
    # geom_line(linetype = "dashed", alpha = .5) +
    geom_point() +
    geom_linerange(aes(ymin = `2.5%`, ymax = `97.5%`), alpha = .5, size = 1.2) +
    scale_y_continuous(limits = if (is.na(ylimits)) {c(0, NA)} else {c(0, ylimits)}) +
    scale_x_date(date_breaks = "2 years", 
                 date_minor_breaks = "1 year", 
                 date_labels = "%Y") +
    guides(x =  guide_axis(angle = 45)) +
    scale_color_manual(values = c_met) +
    labs(title = title, x = NULL, y = NULL) -> tmp
  
  if (percent == TRUE) {tmp + scale_y_percent} else {tmp}
}

cor_plot <- function(col, title = NULL, limits = NA, breaks = NA) {
  
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
                   height = 0, alpha = .5, size = 1.2) +
    geom_errorbar(aes(ymin = !!sym(col_c[1]), ymax = !!sym(col_c[3]), color = year), 
                  width = 0, alpha = .5, size = 1.2) +
    labs(title = title, 
         y = "counting method", x = "sampling method") +
    scale_x_continuous(expand = expansion(mult = c(0, 0)), 
                       limits = if (is.na(limits)) {
                         c(0, 1.1 * max(unlist(tmp[-1])))
                       } else {limits}, 
                       breaks = if (is.na(breaks)) {
                         pretty_breaks()
                       } else {breaks_extended(n = breaks)}) +
    scale_y_continuous(expand = expansion(mult = c(0, 0)), 
                       limits = if (is.na(limits)) {
                         c(0, 1.1 * max(unlist(tmp[-1])))
                       } else {limits}, 
                       breaks = if (is.na(breaks)) {
                         pretty_breaks()
                       } else {breaks_extended(n = breaks)})
}

# Data import -----------------------------------------------------------------------

read_rds("./Data/Ruddy_duck_data.rds") %>% pluck(1) %>% 
  mutate(across(pop, as_factor)) %>% arrange(pop) -> frag

read_rds("./Data/Ruddy_duck_data.rds") %>% pluck(2) %>% 
  mutate(across(pop, as_factor)) %>% arrange(pop) -> counts


# Male proportion -------------------------------------------------------------------

# adult
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
  labs(y = "Male proportion adult")
  
# juvenile
frag %>% 
  filter(age == 'no_ad', sex != "ind") %>% 
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
  labs(y = "Male proportion juvenile")

# global
frag %>% 
  filter(age != "ind", sex != "ind") %>% 
  filter(!(age == "no_ad" & repro == "after_rep")) %>% 
  group_by(pop, sex, age) %>% 
  summarize(across(shot, sum)) %>% 
  group_by(pop, age) %>% 
  mutate(tot = sum(shot)) %>%  
  filter(sex == "mal", tot > 0) %>% 
  group_by(age) %>% 
  mutate(inf = qbeta(0.025, shot, tot - shot), 
         male_proportion = qbeta(0.5, shot, tot - shot), 
         sup = qbeta(0.975, shot, tot - shot))

# Count and sample formatting ------------------------------------------------------

# one keeps only counts from Dec and Jan
# one keeps only samples when total > 100
counts %>% 
  unnest(sex_app) %>% 
  filter(obs_type_mal + obs_type_fem > 0, month(date) %in% c(12, 1)) %>% 
  group_by(across(c(year, pop))) %>% 
  summarize(across(starts_with("obs"), sum)) %>% 
  ungroup() %>% 
  filter(obs_type_mal + obs_type_fem > 100) -> count_sex_app

counts %>% 
  unnest(age) %>% 
  filter(ad + no_ad > 100) %>% 
  select(year, pop, ad, no_ad) -> sample_recruit

counts %>% 
  mutate(year = year + years(1),
         killed_before_rep = killed_before_rep %>% replace_na(0),
         n_breed = count - killed_before_rep) %>% 
  select(year, pop, n_breed) -> count_breed

counts %>% 
  mutate(n_pop = count - killed_before_rep) %>% 
  select(year, n_win = count, n_pop, pop) %>% 
  left_join(count_breed) %>% 
  left_join(count_sex_app) %>% 
  left_join(sample_recruit) %>% 
  arrange(pop, year) -> counts_1

# male and female proportion in samples
# one keeps only years with only few unidentified individuals
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
  filter(!(pop == "FR" & year(year) < 2011), !(pop == "UK" & year(year) > 2009)) %>%
  group_by(sex) %>% 
  summarize(across(shot, sum)) %>% 
  pivot_wider(names_from = sex, values_from = shot) %>% 
  select(kill_f = fem, kill_m = mal) %>% 
  bind_cols(counts_1) %>% 
  relocate(c(n_pop, kill_m, kill_f), .after = n_breed) %>% 
  mutate(obs_tot = obs_type_mal + obs_type_fem,
         spl_tot = ad + no_ad) %>% 
  relocate(obs_tot, .before = ad) %>% 
  rowid_to_column() -> ds

count_sex_app %>% 
  mutate(method = "winter counts",
         tot = obs_type_mal + obs_type_fem,
         `male proportion` = obs_type_mal / tot) %>% 
  select(method, `male proportion`, tot) %>% 
  bind_rows(frag %>%  
              filter(sex != "ind") %>% 
              filter(!(pop == "FR" & year(year) < 2011), 
                     !(pop == "UK" & year(year) > 2009)) %>%
              group_by(year, pop) %>% 
              mutate(tot = sum(shot)) %>%  
              filter(sex == "mal") %>% 
              mutate(mal = sum(shot)) %>% 
              distinct(year, pop, mal, tot) %>%
              ungroup() %>% 
              filter(tot > 50) %>%
              mutate(`male proportion` = mal / tot, 
                     method = "samples") %>%
              select(method, `male proportion`, tot)) %>% 
  mutate(method = as_factor(method)) %>% 
  ggplot(aes(x = method, y = `male proportion`, size = tot)) + 
  geom_jitter(width = 0.2, alpha = .5) +
  scale_y_continuous(limits = c(0, 1), breaks = 0:10/10, labels = scales::percent) +
  guides(size = guide_legend(title = "Number of\ncounts or samples")) +
  xlab("") -> intro_plot; intro_plot

# Lambda formatting ------------------------------------------------------------

# estimation of lambda_max to test recruitment rate reliability
# when ruddy ducks start to be permanently seen and growing?
counts_1 %>% 
  ggplot(aes(x = year, y = n_pop, color = pop)) + 
  geom_line() +
  geom_point() +
  facet_wrap(~ pop, ncol = 1) +
  theme(legend.position = "none") +
  scale_y_log10() +
  scale_x_date_own(1e-2)

# from 1972 in UK, from 1994 in FR
# growth shift from 1981 in UK when pop > 1000

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
              filter(!(age %in% c("no_ad", "ind") & repro == "after_rep")) %>% # 
              group_by(year, pop) %>% 
              summarize(across(shot, sum)) %>% 
              ungroup() %>% 
              filter(shot > 0)) %>%
  mutate(across(shot, replace_na, 0),
         exploitation_rate = (shot / n_win) %>% replace_na(0)) %>% 
  ggplot(aes(x = year, y = exploitation_rate, color = pop)) +
  geom_hline(yintercept = c(.1), linetype = "dashed") +
  geom_point() +
  geom_line() +
  facet_wrap(~ pop, nrow = 2) +
  scale_x_date_own(1e-2) +
  scale_color_manual(values = c_pop) +
  scale_y_continuous(labels = scales::percent, 
                     breaks = scales::pretty_breaks(), 
                     limits = c(0, 1.1))
# from 1999 in UK, and from 2004 in FR si 10% 
# but remove lambda on year 2000 & 2001 because high exploitation in year 1999 & 2000

#lambda dataset formatting

counts_1 %>% 
  mutate(plot1 = if_else(pop == "UK" & year(year) >= 2006 & year(year) <= 2013, 
                         "UK decrease", NA_character_),
         plot2 = if_else(pop == "FR" & year(year) >= 2004 & year(year) <= 2018, 
                         "FR constant", NA_character_),
         plot3 = if_else(pop == "UK" & year(year) >= 1972 & year(year) <= 1981, 
                         "UK small pop.", NA_character_),
         plot4 = if_else(pop == "UK" & year(year) >= 1981 & year(year) <= 1999, 
                         "UK large pop.", NA_character_),
         plot5 = if_else(
           (pop == "FR" & year(year) >= 1994 & year(year) <= 1999) | 
             (pop == "FR" & year(year) >= 2001 & year(year) <= 2004), 
             "FR small pop.", NA_character_)) -> counts_2

1:5 %>% 
  map(~ counts_2 %>% 
        filter(!is.na(!! sym(str_c("plot", .x)))) %>% 
        rename(sub_pop = !! sym(str_c("plot", .x))) %>% 
        select(year, pop, sub_pop, n_pop) %>% 
        mutate(across(n_pop, log))) %>% 
  bind_rows() %>%
  mutate(across(sub_pop, as_factor),
         sub_ts = as.numeric(sub_pop) + 
           if_else(sub_pop == "FR small pop." & 
                     pop == "FR" & 
                     year(year) >= 2001, 1, 0)) %>% 
  relocate(sub_ts, .after = sub_pop) -> lambda_ds

# avg lambda plotting

ds %>% 
  ggplot(aes(x = year, y = n_pop)) + 
  facet_wrap( ~ pop, ncol = 1) +
  geom_line(alpha = 0.5, linetype = "dashed") +
  geom_point(shape = 16, color = "gray") +
  scale_y_log10(minor_breaks = NULL) +
  annotation_logticks(side = "l", color = "grey") +
  geom_point(data = lambda_ds %>% 
               mutate(n_pop = exp(n_pop)),
             aes(x = year, y = n_pop, color = sub_pop)) +
  scale_x_date_own(1e-2)

# Data formatting functions ---------------------------------------------------------

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

lam <- function(gp = NA, col = NA, size = FALSE) {
  gp = enquo(gp)
  col = enquo(col)
  x <- lambda_ds %>% 
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
    
    # hierarchical prior
    for (i in 1:(C_id_max + S_id_max)) {
      shape1[i] ~ dunif(1, 1e3)
      shape2[i] ~ dunif(1, 1e3)
      
      prior[i] ~ dbeta(shape1[i], shape2[i])
    }
    
    for (j in 1:C) {
      
      # p_mal_ad prior
      # p_mal_ad[j] ~  dbeta(shape1[C_id[j]], shape2[C_id[j]])
      p_mal_ad[j] ~  dbeta(1, 1)
      
      # LL
      cnt_mal_ad[j] ~ dbin(p_mal_ad[j], cnt_tot[j])
      
      p_juv[j] <- (1 - p_mal_ad[j] / p_mal) # * 1.741913
      
      # recruitment and recruitment rate estimation
      recruits[j] <- p_juv[j] * cnt_size_pop[j]
      
      productivity[j] <- recruits[j] / cnt_size_breeding_pop[j]
      
      survival[j] <- cnt_size_pop[j] / cnt_size_breeding_pop[j] - productivity[j]
      
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
      
      survival[C + k] <- spl_size_pop[k] / spl_size_breeding_pop[k] - productivity[C + k]
      
    }
    
    # r estimation from counts
    
    # priors
     max_surv ~ dunif(.7, .9)
    
    for (r in 1:R_id_max) {
      
      r_avg[r] ~ dunif(log(0.01), log(5))
      N_sd[r] ~ dunif(0, 1e3)
      log(lambda[r]) <- r_avg[r]
      max_prod[r] <- lambda[r] - max_surv
    }
    
    for (ts in 1:TS_id_max) {
      
      log_N_int[ts] ~ dnorm(0, sd = 1e2)
    }
    
    # LL
    for (t in 1:R) {
      
      r_avg_size_pop[t] ~ dnorm(log_N_int[TS_id[t]] + r_avg[R_id[t]] * year[t], 
                                sd = N_sd[R_id[t]])
    }
    
  })

# set seed
myseed <- 1
set.seed(myseed)

# model data
JuvConst <- list(C = dis(obs_tot, pop, id, TRUE),
                 C_id = dis(obs_tot, pop, id),
                 C_id_max = dis(obs_tot, pop, id) %>% max(),
                 S = dis(spl_tot, pop, id, TRUE),
                 S_id = dis(spl_tot, pop, id),
                 S_id_max = dis(spl_tot, pop, id) %>% max(),
                 R = lam(sub_pop, id, TRUE),
                 R_id = lam(sub_pop, id),
                 R_id_max = lam(sub_pop, id) %>% max(),
                 TS_id = lam(sub_ts, id),
                 TS_id_max = lam(sub_ts, id) %>% max())

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
                r_avg_size_pop = lam(sub_pop, n_pop),
                year = lambda_ds %>% 
                  group_by(sub_ts) %>% 
                  mutate(mi_ye = min(year),
                         year = year(year) - year(mi_ye)) %>% 
                  pull(year))

JuvInit <- list(p_mal = 0.6,
                p_mal_ad = rep(.1, JuvConst$C),
                shape1 = rep(1, 3),
                shape2 = rep(1, 3),
                prior = rep(.1, 3),
                p_juv = rep(.1, JuvConst$C + JuvConst$S),
                log_N_int = rep(0, 6),
                max_surv = .9,
                r_avg = rep(log(.5), 5),
                N_sd = rep(1, 5))

# node targets
JuvMon <- c("p_mal",
            "prior",
            "p_juv",
            "recruits",
            "productivity",
            "survival",
            "lambda",
            "max_prod",
            "log_N_int")

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
         parms = JuvMon[7]) 

# convergence check + vizualisation of prior update
MCMCtrace(object = JuvOut, 
          params = JuvMon[7], 
          iter = nsample_1,
          # priors = runif(nchain_1 * nsample_1, 0, 1), 
          pdf = FALSE,
          ind = TRUE,
          Rhat = TRUE)

# prod, r_avg, survival plot
MCMCplot(JuvOut, 
         params = JuvMon[7],
         horiz = FALSE, 
         ylim = c(0, 2))

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
  rowwise() %>% 
  mutate(id = var %>% str_extract("\\d+") %>% as.numeric(),
         par = var %>% str_extract(".*(?=\\[)"),
         par = if_else(is.na(par), var, par),
         rowid = case_when(
           par %in% 
             c("p_juv", 
               "recruits", 
               "productivity", 
               "survival") ~ c(dis(obs_tot, pop, rowid), dis(spl_tot, pop, rowid))[id],
           TRUE ~ NA_integer_)) %>% 
  ungroup() %>% 
  arrange(par, id) %>% 
  mutate(method = case_when(
    par %in% 
      c("p_juv", 
        "recruits", 
        "productivity", 
        "survival") ~ c(rep("counting", dis(obs_tot, pop, id, TRUE)), 
                        rep("sampling", dis(spl_tot, pop, id, TRUE)))[id],
    par %in% c("lambda", "max_prod") ~ levels(lam(sub_pop, sub_pop))[id],
    par == "log_N_int" ~ as.character(id),
    TRUE ~ NA_character_)) %>% 
  left_join(ds %>% select(rowid, year, pop)) %>% 
  select(year, pop, par, method, `2.5%`, `50%`, `97.5%`) -> JuvOut3

# average recruitment rate upon pop, method, & harvests
JuvOut3 %>% 
  filter(par == "productivity") %>% 
  rowid_to_column("id") %>% 
  mutate(name = str_c(par, "[", id, "]"), 
         harvest = if_else(pop == "FR" & year(year) %in% c(1999, 2002:2004),
           "no harvest", "harvest")) %>% 
  select(year, pop, method, harvest, name) -> sel

list(
  JuvOut3 %>% 
    filter(par == "max_prod", 
           method %>% str_detect("small")) %>% 
    mutate(pop = method %>% str_sub(1, 2) %>% as_factor(), 
           method = "proxy") %>% 
    select(pop, method, contains("%")),
  
  sel %>% 
    group_split(pop, method, harvest) %>% 
    map(function(x) {
      prod = as_tibble(JuvOut) %>%
        select(x %>% pull(name)) %>%
        rowMeans()
      prod_avg = quantile(prod, c(.025, .5, .975))
      pro = tibble(prod_avg, name = names(prod_avg)) %>% 
        pivot_wider(names_from = name, values_from = prod_avg)
      out = x %>% 
        distinct(pop, method, harvest) %>% 
        bind_cols(pro) %>% 
        mutate(method = interaction(method, harvest)) %>% 
        select(-harvest)
      return(out)
    }) %>% 
    bind_rows()
  ) %>% 
  bind_rows() %>% 
  mutate(par = "prod_avg") -> avg

bind_rows(JuvOut3, avg) -> JuvOut4

lambda_ds %>% 
  left_join(JuvOut3 %>% 
              filter(par == "lambda") %>% 
              mutate(r_avg = log(`50%`)) %>% 
              select(sub_pop = method, r_avg)) %>% 
  left_join(JuvOut3 %>% 
              filter(par == "log_N_int") %>% 
              mutate(sub_ts = as.numeric(method)) %>% 
              select(sub_ts, intercept = `50%`)) %>% 
  group_by(sub_ts) %>% 
  mutate(y = intercept + r_avg * (year(year) - min(year(year))),
         sub_pop = sub_pop %>% as_factor(), 
         lambda = exp(r_avg)) %>% 
  ungroup() -> lambda_ds_2

JuvOut4 %>% 
  filter(par == "productivity", method == "counting") %>% 
  mutate(gp = if_else(year(year) %in% c(1999, 2002:2004) & pop == "FR", 
                      "no_harvest", "harvest")) %>% 
  group_by(pop, gp) %>% 
  mutate(mean = mean(`50%`)) %>% 
  ungroup() %>% 
  mutate(res = `50%` - mean) %>% 
  group_by(pop) %>% 
  mutate(var_tot = var(`50%`),
         var_res = var(res), 
         prop_var = (var_tot - var_res) / var_tot)

JuvOut4 %>% 
  filter(par == "survival", method == "counting") %>% 
  mutate(gp = if_else(year(year) %in% c(1999, 2002:2004) & pop == "FR", 
                      "no_harvest", "harvest")) %>% 
  group_by(pop, gp) %>% 
  mutate(mean = mean(`50%`)) %>% 
  ungroup() %>% 
  mutate(res = `50%` - mean) %>% 
  group_by(pop) %>% 
  mutate(var_tot = var(`50%`),
         var_res = var(res), 
         prop_var = (var_tot - var_res) / var_tot)

# Output plot -----------------------------------------------------------------------

raw_plot(p_juv, title = "Proportion of recruits in the population", 
         percent = TRUE) -> raw_prop; raw_prop

raw_plot(productivity, title = "Productivity: number of recruits per breeder") + 
  scale_y_continuous(breaks = 0:12/10, limits = c(0, 1.2)) -> raw_prod; raw_prod

raw_plot(survival, title = "Adult survival", 
         percent = TRUE) + 
  scale_y_continuous(breaks = 0:12/10, limits = c(0, 1.2)) +
  geom_hline(yintercept = 1, linetype = "dashed") -> raw_surv; raw_surv

raw_plot(recruits, title = "Number of recruits") + 
  facet_wrap(~ pop, nrow = 1, scales = "free") -> raw_recruit; raw_recruit

JuvOut4 %>% 
  filter(!is.na(method), par == "productivity") %>% 
  select(year, pop, method, `50%`) %>% 
  pivot_wider(names_from = method, values_from = `50%`) %>% 
  filter(!is.na(sampling), !is.na(counting)) -> tls1_ds

x1 <- tls(counting ~ 0 + sampling, data = tls1_ds)$coefficient

JuvOut4 %>% 
  filter(!is.na(method), par == "survival") %>% 
  select(year, pop, method, `50%`) %>% 
  pivot_wider(names_from = method, values_from = `50%`) %>% 
  filter(!is.na(sampling), !is.na(counting)) -> tls2_ds

x2 <- tls(counting ~ 0 + sampling, data = tls2_ds)$coefficient

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

cor_plot(col = "recruits", title = "Number of recruits") -> cor_recruit; cor_recruit

cor_plot(col = "productivity", 
         title = "Productivity: number of recruits per breeder",
         limits = c(0, .7)) +
  geom_segment(x = .55, y = .55, xend = .55, yend = x1 * .55,
               arrow = arrow(type = "closed", length = unit(0.3, "cm")),
               size = .5, color = "#de2d26") +
  geom_text(x = .55, y = .65, 
            label = "1:1", 
            color = "black",
            hjust = 1) +
  geom_text(x = .55, y = .45, 
            label = str_c(" - ", round(1e2 * (1 - x1), 1), "% "), 
            color = "#de2d26",
            hjust = 0) -> cor_prod; cor_prod

cor_plot(col = "survival", 
         title = "Survival rate",
         limits = c(0, 1)) +
  geom_segment(x = .65, y = .65, xend = .65, yend = x2 * .65,
               arrow = arrow(type = "closed", length = unit(0.3, "cm")),
               size = .5, color = "#de2d26") +
  geom_text(x = .95, y = .80, 
            label = "1:1", 
            color = "black",
            hjust = 1) +
  geom_text(x = .45, y = .90, 
            label = str_c(" + ", round(-1e2 * (1 - x2), 1), "% "), 
            color = "#de2d26",
            hjust = 0) -> cor_surv; cor_surv

ds %>% 
  ggplot(aes(x = year, y = n_pop)) + 
  facet_wrap( ~ pop, ncol = 1) +
  geom_line(alpha = 0.5, linetype = "dashed") +
  geom_point(shape = 16, color = "gray") +
  scale_y_log10(minor_breaks = NULL) +
  annotation_logticks(side = "l", color = "grey") +
  geom_point(data = lambda_ds %>% 
               mutate(n_pop = exp(n_pop)), 
             aes(x = year, y = n_pop, color = sub_pop)) +
  scale_x_date_own(1e-2) +
  geom_line(data = lambda_ds_2, 
            aes(x = year, y = exp(y), 
                group = sub_ts, color = sub_pop))

# Max growth rate proxy --------------------------------------------------------------

JuvOut4 %>% 
  filter(par == "prod_avg") %>% 
  mutate(box = case_when(
    method %>% str_detect("proxy") ~ "proxy of maximum\nrecruitment rate",
    method %>% str_detect(".no|sampling") ~ "recruitment rate\nwithout harvest",
    TRUE ~ "recruitment rate\nwith harvest") %>% 
      as_factor() %>% 
      fct_relevel("recruitment rate\nwith harvest", after = Inf)) %>% 
      arrange(pop, box) %>% 
  mutate(across(contains("%"), ~ if_else(str_detect(method, "sampling"), NA_real_, .x)),
         txt = if_else(str_detect(method, "sampling"), "NO DATA", NA_character_)) %>% 
  ggplot(aes(x = pop, y = `50%`, color = pop)) +
  facet_wrap(~ box, nrow = 1) +
  geom_point() +
  geom_linerange(aes(ymin = `2.5%`, ymax = `97.5%`), alpha = .5, size = 1.2) +
  geom_text(aes(x = pop, y = .5, label = txt), color = c_pop[1], angle = 90) +
  scale_color_manual(values = c_pop, guide = "none") +
  scale_y_continuous(
    limits = c(0, 1),
    breaks = 0:10 / 10) +
  labs(x = NULL, y = "average recruitment rate") -> prod_max_plot; prod_max_plot

# Plots for article ------------------------------------------------------------------

# Time series
JuvOut3 %>%
  filter(par == "productivity") %>% 
  group_split(pop, method) %>% 
  map(~ .x %>% 
        mutate(diff = c(1, diff(year(year))) - 1, 
               diff = if_else(diff != 0, 1, 0), 
               diff = cumsum(diff)) %>% 
        group_by(pop, method, diff) %>% 
        summarize(xmin = min(year) - months(6),
                  xmax = max(year) + months(6)) %>% 
        ungroup()) %>%
  bind_rows() %>% 
  mutate(label = str_c(method, " time series")) %>% 
  select(pop, label, xmin, xmax) %>% 
  bind_rows(
    ds %>% 
      mutate(
        rec = case_when(
          pop == "UK" & year(year) < 1999 ~ 1,
          pop == "UK" & year(year) >= 1999 ~ 2,
          pop == "FR" & year(year) < 1999 ~ 3, 
          pop == "FR" & year(year) %in% c(1999:2000) ~ 4,
          pop == "FR" & year(year) %in% c(2001:2003) ~ 5,
          pop == "FR" & year(year) >= 2004 ~ 6),  
        label = if_else(rec %in% c(1, 3, 5), 
                        "no harvest", 
                        "harvest")) %>% 
      group_by(rec, label, pop) %>% 
      summarize(xmin = min(year),
                xmax = max(year) + years(1)) %>% 
      rowwise() %>% 
      mutate(xmax = min(xmax, ymd(20210101))) %>% 
      ungroup() %>% 
      select(pop, label, xmin, xmax)) %>% 
  rowwise() %>% 
  mutate(label = factor(label, 
                        levels = c("counting time series", 
                                   "sampling time series",
                                   "harvest",
                                   "no harvest")) %>% 
           fct_relevel("harvest", after = Inf),
         ymin = 10^(log10(6.5e3) + 0.1 * min(c(2, (as.numeric(label) - 1)))),
         ymax = 10^(log10(ymin) + 0.1)) -> time_series

ds %>% 
  ggplot(aes(x = year, y = n_pop)) + 
  facet_wrap( ~ pop, ncol = 1) +
  geom_line(alpha = 0.5, linetype = "dashed") +
  geom_point(shape = 16, color = "gray") +
  scale_y_log10(minor_breaks = NULL) +
  annotation_logticks(side = "l", color = "grey") +
  scale_x_date_own(1e-2) +
  scale_shape_manual(values = 16, guide = "none") +
  scale_color_manual(values = c_pop, guide = "none") +
  labs(y = "Breeding population size") +
  geom_rect(data = time_series,
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = label), 
            alpha = .8, 
            inherit.aes = FALSE) + 
  scale_fill_manual(values = c(c_met, "#CCCCCC", "#333333")) +
  guides(fill = guide_legend(title = "")) +
  theme(legend.position = "bottom") -> matmet; matmet

# avg lambda plotting

ds %>% 
  ggplot(aes(x = year, y = n_pop)) + 
  facet_wrap( ~ pop, ncol = 1) +
  geom_line(data = lambda_ds_2 %>% 
              filter(sub_pop %>% str_detect("pop.")),
            aes(x = year, y = exp(y), 
                group = sub_ts, color = pop), size = 2, alpha = 0.3) +
  geom_line(alpha = 0.5, linetype = "dashed") +
  geom_point(shape = 16, color = "gray") +
  scale_y_log10(minor_breaks = NULL) +
  annotation_logticks(side = "l", color = "grey") +
  geom_point(data = lambda_ds %>% 
               filter(sub_pop %>% str_detect("pop.")) %>% 
               mutate(n_pop = exp(n_pop)), 
             aes(x = year, y = n_pop, color = pop, alpha = sub_pop)) +
  scale_x_date_own(1e-2) +
  scale_shape_manual(values = 16, guide = "none") +
  scale_color_manual(values = c_pop, guide = "none") +
  scale_alpha_manual(values = c(1, .5, 1), guide = "none") +
  labs(y = "Breeding population size") +
  geom_rect(data = time_series,
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = label), 
            alpha = .8, 
            inherit.aes = FALSE) + 
  scale_fill_manual(values = c(c_met, "#CCCCCC", "#333333")) +
  guides(fill = guide_legend(title = "")) +
  theme(legend.position = "bottom") -> lambda_max; lambda_max

JuvOut4 %>% 
  filter(par == "lambda", str_detect(method, "pop.")) %>% 
  mutate(pop = as_factor(str_trunc(method, 2, ellipsis = "")),
         sub_pop = as_factor(method)) %>% 
  ggplot(aes(x = sub_pop, y = `50%` - 1, group = sub_pop, color = pop, alpha = sub_pop)) +
  geom_point(size = 2) +
  geom_linerange(aes(ymin = `2.5%` - 1, ymax = `97.5%` - 1), alpha = .5, size = 1.2) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1L, prefix = "+"),
    limits = c(0, .8),
    breaks = 0:10 / 10) +
  scale_alpha_manual(values = c(1, .5, 1), guide = "none") +
  scale_color_manual(values = c_pop, guide = "none") +
  labs(title = bquote(
    'Population growth without harvest (' * lambda * ' - 1)'),
    y = NULL, x = NULL) -> lambda_max_plot; lambda_max_plot

# Save ------------------------------------------------------------------------------

grid.arrange(
  grobs = list(raw_prod + 
                 labs(title = "A", y = "recruitment rate") + 
                 theme(legend.position = "bottom"),
               cor_prod + labs(title = "B")),
  width = c(5, 3),
  heights = c(4, 1),
  layout_matrix = rbind(c(1, 2),
                        c(1, NA))) -> prod_res

grid.arrange(
  grobs = list(raw_surv + 
                 labs(title = "A", y = "adult survival rate") + 
                 theme(legend.position = "bottom"),
               cor_surv + labs(title = "B")),
  width = c(5, 3),
  heights = c(4, 1),
  layout_matrix = rbind(c(1, 2),
                        c(1, NA))) -> surv_res

grid.arrange(
  grobs = list(
    lambda_max + 
      geom_text(data = lambda_ds_2 %>%
                  filter(!sub_ts %in% c(1:2)) %>%
                  group_by(pop, lambda) %>%
                  summarize(across(year, mean),
                            across(y, ~ exp(max(.)))) %>%
                  ungroup() %>% 
                  mutate(lambda = round(1e2 * (lambda - 1)) %>% 
                           str_c("+", ., "% per year"),
                         y = c(6e3, 4e2, 5e2),
                         year = ymd(c(19890101, 19680101, 19980101))),
                aes(x = year, y = y, label = lambda, color = pop)) +
      theme(legend.position = "bottom") +
      labs(title = "A"), 
    lambda_max_plot + 
      guides(x = guide_axis(angle = 45)) +
      labs(title = "B",
           y = bquote(
             'Population growth (' * lambda * ' - 1)'))),
  widths = c(5, 2),
  heights = c(5, 3), 
  layout_matrix = rbind(c(1, 2),
                        c(1, NA))) -> raw_pop_res

prod_max_plot

grid.arrange(
  grobs = list(raw_prop + theme(legend.position = "none") + 
                 labs(title = "A1", y = "immature proportion"),
               cor_prop + labs(title = "A2"),
               raw_recruit + theme(legend.position = "none") + 
                 labs(title = "B1", y = "recruitment"),
               cor_recruit + labs(title = "B2"),
               get_legend(raw_recruit + theme(legend.position = "bottom"))),
  widths = c(5, 3),
  heights = c(6, 6, 1),
  layout_matrix = rbind(c(1, 2),
                        c(3, 4), 
                        c(5, NA))) -> annex

list(intro_plot,
     matmet,
     lambda_max, 
     prod_res,
     surv_res,
     raw_pop_res,
     prod_max_plot,
     annex) -> plo

list(c(4, 4),
     c(7, 5),
     c(7, 5),
     c(9, 4.5),
     c(9, 4.5),
     c(10, 5),
     c(6, 4),
     c(9.5, 6)) -> plo_dim

list(plo, c(1:length(plo)), plo_dim) %>% 
  pwalk(~ ggsave(str_c("./Output/plot_", ..2, ".png"),
                 ..1, 
                 width = ..3[1], 
                 height = ..3[2], dpi = 600))
