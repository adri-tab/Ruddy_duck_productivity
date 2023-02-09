# Loading packages -------------------------------------------------------------------

Sys.setlocale("LC_ALL", "en_US.UTF-8")

require(tidyverse)
require(rlang)
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
c_met <- c("#a1d99b", "#41ab5d")

# Plot functions --------------------------------------------------------------------

scale_y_percent <- function() {
  list(
  scale_y_continuous(
  labels = scales::percent_format(accuracy = 1L),
  limits = c(0, 1),
  breaks = 0:10 / 10, 
  minor_breaks = NULL)
  )}

scale_x_date_own <- function(coef = 5e-2) {
  list(
    scale_x_date(date_breaks = "2 years", 
                 date_minor_breaks = "1 year", 
                 date_labels = "%Y", 
                 expand = c(coef, coef)),
    guides(x =  guide_axis(angle = 45)),
    labs(x = NULL) 
  )}

cor_plot <- function(col, title = NULL, subtitle = NULL, limits = NA, breaks = NA, one = 1) {
  
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
                color = "gray", linewidth = 1.3) +
    geom_abline(slope = 1, linetype = "dashed", alpha = one) +
    geom_point(aes(color = year)) +
    geom_errorbarh(aes(xmin = !!sym(col_s[1]), xmax = !!sym(col_s[3]), color = year), 
                   height = 0, alpha = .5, linewidth = 1.2) +
    geom_errorbar(aes(ymin = !!sym(col_c[1]), ymax = !!sym(col_c[3]), color = year), 
                  width = 0, alpha = .5, linewidth = 1.2) +
    labs(title = title,
         subtitle = subtitle,
         y = "count-based method", x = "hunting-bag-based method") +
    scale_x_continuous(expand = expansion(mult = c(0, 0)), 
                       limits = if (any(is.na(limits))) {
                         c(0, 1.1 * max(unlist(tmp[-1])))
                       } else {limits}, 
                       breaks = if (is.na(breaks)) {
                         pretty_breaks()
                       } else {breaks_extended(n = breaks)}) +
    scale_y_continuous(expand = expansion(mult = c(0, 0)), 
                       limits = if (any(is.na(limits))) {
                         c(0, 1.1 * max(unlist(tmp[-1])))
                       } else {limits}, 
                       breaks = if (is.na(breaks)) {
                         pretty_breaks()
                       } else {breaks_extended(n = breaks)})
}

# Data import -----------------------------------------------------------------------

read_rds("./Data/Ruddy_duck_data.rds") %>% pluck(1) %>% 
  mutate(pop = if_else(pop == "UK", "GB", pop),
         across(pop, as_factor)) %>% arrange(pop) -> frag

read_rds("./Data/Ruddy_duck_data.rds") %>% pluck(2) %>% 
  mutate(pop = if_else(pop == "UK", "GB", pop),
         across(pop, as_factor)) %>% arrange(pop) -> counts


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
  scale_y_percent() +
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
  scale_y_percent() +
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
         n_breed = count) %>% 
  #          n_breed = count - killed_before_rep) %>%  # if reference is the breeding time
  select(year, pop, n_breed) -> count_breed

counts %>% 
  mutate(n_pop = count) %>% 
  #   mutate(n_pop = count - killed_before_rep) %>% 
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
  filter(!(pop == "FR" & year(year) < 2011), !(pop == "GB" & year(year) > 2009)) %>%
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

## intro_plot ------------------------------------------------------------------------

count_sex_app %>% 
  mutate(method = "Female %\nin counts",
         tot = obs_type_mal + obs_type_fem,
         `female proportion` = obs_type_fem / tot) %>% 
  select(method, `female proportion`, tot) %>% 
  bind_rows(frag %>%  
              filter(sex != "ind") %>% 
              filter(!(pop == "FR" & year(year) < 2011), 
                     !(pop == "GB" & year(year) > 2009)) %>%
              group_by(year, pop) %>% 
              mutate(tot = sum(shot)) %>%  
              filter(sex == "fem") %>% 
              mutate(fem = sum(shot)) %>% 
              distinct(year, pop, fem, tot) %>%
              ungroup() %>% 
              filter(tot > 50) %>%
              mutate(`female proportion` = fem / tot, 
                     method = "Female %\nin hunting bags") %>%
              select(method, `female proportion`, tot)) %>% 
  mutate(method = as_factor(method)) %>% 
  ggplot(aes(x = method, y = `female proportion`, size = tot)) + 
  geom_jitter(width = 0.2, alpha = .5) +
  scale_y_continuous(name = "", limits = c(0, 1), breaks = 0:10/10, labels = scales::percent) +
  guides(size = guide_legend(title = "Sample size")) +
  xlab("") + 
  # labs(title = "Female proportion", caption = "data from GB & French Ruddy duck populations") +
  coord_flip() -> intro_plot; intro_plot

# Observed harvest rate ----------------------------------------------------------

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

# from 1972 in GB, from 1994 in FR
# growth shift from 1981 in GB when pop > 1000

# when exploitation starts and prevents from estimating lambda_max
frag %>% 
  mutate(age = if_else(age != "ind", "known", "ind")) %>% 
  group_by(year, pop, age) %>% 
  summarise(across(shot, sum)) %>% 
  left_join(counts_1 %>% select(year, pop, n_win)) %>% 
  ggplot(aes(x = year)) +
  geom_col(aes(y = shot, fill = age)) +
  geom_line(aes(y = n_win)) +
  facet_wrap( ~ pop, ncol = 1, scales = "free_y") +
  scale_x_date_own(0)

frag %>% 
  filter(age != "ind", pop == "FR", year(year) %in% c(2009:2020)) %>% 
  group_by(year, age) %>% 
  summarize(across(shot, sum)) %>% 
  group_by(year) %>% 
  mutate(ratio = round(100 * shot / sum(shot))) %>% 
  filter(age == "ad") %>% 
  pull(ratio) %>% 
  mean() -> p_ad; p_ad
  
frag %>%
  group_by(year, pop, age) %>% 
  summarise(across(shot, sum)) %>% 
  left_join(counts_1 %>% select(year, pop, n_win)) %>% 
  ggplot(aes(x = year)) +
  geom_col(aes(y = shot, fill = age)) +
  geom_line(aes(y = n_win)) +
  facet_wrap( ~ pop, ncol = 1, scales = "free_y") +
  scale_x_date_own(0)
  

frag %>%  
  mutate(shot = shot * if_else(age == "ind", 1e-2 * p_ad, 1)) %>% 
  filter(age != "no_ad") %>% 
  group_by(year, pop, age) %>% 
  summarise(across(shot, sum)) %>% 
  left_join(counts_1 %>% select(year, pop, n_win)) %>% 
  ggplot(aes(x = year)) +
  geom_col(aes(y = shot, fill = age)) +
  geom_line(aes(y = n_win)) +
  facet_wrap( ~ pop, ncol = 1, scales = "free_y") +
  scale_x_date_own(0)

frag %>% 
  mutate(shot = shot * if_else(age == "ind", 1e-2 * p_ad, 1)) %>% 
  filter(age != "no_ad") %>% # 
  group_by(year, pop) %>% 
  summarize(across(shot, sum)) %>% 
  ungroup() %>% 
  filter(shot > 0) -> frag_cor

frag %>% 
  filter(age == "ad") %>% # 
  group_by(year, pop) %>% 
  summarize(across(shot, sum)) %>% 
  ungroup() %>% 
  filter(shot > 0) -> frag_uncor

## harv_plot ------------------------------------------------------------------

counts_1 %>% 
  filter(year(year) <= 2017) %>% 
  left_join(frag_cor) %>%
  mutate(across(shot, ~ replace_na(.x, 0)),
         harvest_rate = (shot / n_win) %>% replace_na(0)) %>% 
  ggplot(aes(x = year, y = harvest_rate, color = pop)) +
  # geom_smooth(method = "loess") +
  geom_vline(xintercept = c(ymd(19980701), ymd(20050701)), linetype = "dashed") +
  geom_point(alpha = .7) +
  geom_line(linetype = "dashed", alpha = .8) +
  scale_x_date_own(1e-2) +
  scale_color_manual(name = "", 
                     labels = c("GB population", "FR population"), 
                     values = c_pop) +
  scale_y_continuous(name = "adult harvest rate",
                     labels = scales::percent, 
                     breaks = scales::pretty_breaks(), 
                     limits = c(0, 1)) +
  annotate(geom = "text",
           x = c(ymd(19790101), ymd(20020101), ymd(20130101)),
           y = rep(.95, 3), 
           label = c("no\nharvest", "low\nharvest", "high\nharvest"),
           size = 4) -> harv_plot; harv_plot

frag %>% 
  filter(age == "ad") %>% 
  group_by(pop, year) %>% 
  mutate(tot_shot = sum(shot)) %>%  
  group_by(pop, year, repro, tot_shot) %>% 
  summarize(across(shot, sum)) %>% 
  ungroup() %>% 
  filter(shot > 0) %>% 
  filter(year(year) >= 2006, year(year) <= 2017, repro %>% str_detect("before")) %>% 
  group_by(pop) %>% 
  summarize(prop = sum(shot) / sum(tot_shot))

counts_1 %>% 
  filter(year(year) < 2021) %>% 
  left_join(frag_cor) %>%
  mutate(across(shot, ~ replace_na(.x, 0)), # or \(x) replace(x, 0)
         harvest_rate = (shot / n_win) %>% replace_na(0)) %>% 
  filter(year(year) >= 2006, 
         (pop == "FR" & year(year) <= 2017) | (pop == "GB" & year(year) <= 2012)) %>% 
  glm(harvest_rate ~ pop, data = .) %>% 
  summary()

counts_1 %>% 
  filter(year(year) < 2021) %>% 
  left_join(frag_cor) %>%
  mutate(across(shot, ~ replace_na(.x, 0)), # or \(x) replace(x, 0)
         harvest_rate = (shot / n_win) %>% replace_na(0)) %>% 
  filter(year(year) >= 2006, 
         (pop == "FR" & year(year) <= 2017) | (pop == "GB" & year(year) <= 2012)) %>% 
  glm(harvest_rate ~ 1, data = .) %>% 
  summary()

counts_1 %>% 
  filter(year(year) < 2021) %>% 
  left_join(frag_cor) %>%
  mutate(across(shot, ~ replace_na(.x, 0)), # or \(x) replace(x, 0)
         harvest_rate = (shot / n_win) %>% replace_na(0)) %>% 
  filter(year(year) >= 2006, 
         (pop == "FR" & year(year) <= 2017) | (pop == "GB" & year(year) <= 2012)) %>% 
  group_by(pop) %>% 
  summarize(across(harvest_rate, mean))


# Lambda formatting  --------------------------------------------------------------------------


counts_1 %>% 
  mutate(plot1 = if_else(pop == "GB" & year(year) >= 1972 & year(year) <= 1981, 
                         "GB small pop.", NA_character_),
         plot2 = if_else(pop == "FR" & year(year) >= 1994 & year(year) <= 1999,
           "FR small pop.", NA_character_),
         plot3 = if_else(pop == "GB" & year(year) >= 1981 & year(year) <= 1999, 
                         "GB large pop.", NA_character_),
         plot4 = if_else(pop == "GB" & year(year) >= 2006 & year(year) <= 2012, # data time series
                         "GB decrease", NA_character_),
         plot5 = if_else(pop == "FR" & year(year) >= 2006 & year(year) <= 2018, # data time series
                         "FR constant", NA_character_)) -> counts_2

1:5 %>% 
  map(~ counts_2 %>% 
        filter(!is.na(!! sym(str_c("plot", .x)))) %>% 
        rename(sub_pop = !! sym(str_c("plot", .x))) %>% 
        select(year, pop, sub_pop, n_pop) %>% 
        mutate(across(n_pop, log))) %>% 
  bind_rows() %>%
  mutate(across(sub_pop, as_factor),
         sub_ts = as.numeric(sub_pop)) %>%
  relocate(sub_ts, .after = sub_pop) -> lambda_ds

# avg lambda plotting

ds %>% 
  ggplot(aes(x = year, y = n_pop, group = pop)) + 
  scale_y_log10() +
  geom_line(alpha = 0.5, linetype = "dashed") +
  geom_point(shape = 16, color = "gray") +
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
    
    # prior on hierarchical prior on p_mal_ad and p_juv
    for (i in 1:(C_id_max + S_id_max)) {
      shape1[i] ~ dunif(1, 1e3)
      shape2[i] ~ dunif(1, 1e3)
      
      prior[i] ~ dbeta(shape1[i], shape2[i])
    }
    
    for (j in 1:C) {
      
      # hierarchical prior on p_mal_ad
      # p_mal_ad[j] ~  dbeta(shape1[C_id[j]], shape2[C_id[j]])
      
      # non hierarchical prior on p_mal_ad
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
      
      # hierarchical prior on p_juv
      # p_juv[C + k] ~ dbeta(shape1[C_id_max + S_id[k]], shape2[C_id_max + S_id[k]])
      
      # non hierarchical prior on p_juv
      p_juv[C + k] ~ dbeta(1, 1)
      
      #LL
      spl_juv[k] ~ dbin(p_juv[C + k], spl_tot[k])
      
      # recruitment and productivity estimation
      recruits[C + k] <- p_juv[C + k] * spl_size_pop[k]
      
      productivity[C + k] <- recruits[C + k] / spl_size_breeding_pop[k]
      
      survival[C + k] <- spl_size_pop[k] / spl_size_breeding_pop[k] - productivity[C + k]
      
    }
    
    # r estimation from counts
    
    # hierarchical prior on lambda max
    r_max_avg ~ dnorm(log(1.5), sd = 1e1)
    r_max_sd ~ dunif(0, 1e2)
    
    prior_r_max ~ dnorm(r_max_avg, sd = r_max_sd)
    log(prior_lambda_max) <- prior_r_max
    
    for (r in 1:2) {
    # hierarchical prior on r_max
       r_avg[r] ~ dnorm(r_max_avg, sd = r_max_sd)
    # non hierarchical prior on r_max
      # r_avg[r] ~ dunif(log(0.01), log(5))
    }
    
    for (r in 3:R_id_max) {
      r_avg[r] ~ dunif(log(0.01), log(5))
    }
    
    for (r in 1:R_id_max) {
      
      N_sd[r] ~ dunif(0, 1e3)
      max_surv[r] ~ dunif(.7, 1)
      
      log(lambda[r]) <- r_avg[r]
      max_prod[r] <- lambda[r] - max_surv[r]
      max_p_juv[r] <- (lambda[r] - max_surv[r]) / lambda[r]
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
                log_N_int = rep(0, 5),
                max_surv = rep(.85, 5),
                r_max_avg = log(1.5), 
                r_max_sd = 1, 
                prior_r_max = log(1.5),
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
            "prior_lambda_max",
            "r_max_avg",
            "r_max_sd",
            "max_p_juv",
            "max_surv",
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
         parms = JuvMon[7:10]) 

# # convergence check + visualization of prior update
MCMCtrace(object = JuvOut,
          params = JuvMon[9:10],
          iter = nsample_1,
          priors = matrix(
            c(rnorm(nchain_1 * nsample_1, log(1.5), sd = 1e2),
              runif(nchain_1 * nsample_1, 0, 1e2)), 
            ncol = 2, byrow = FALSE),
          pdf = FALSE,
          ind = TRUE,
          Rhat = TRUE)

# # prod, r_avg, survival plot
# MCMCplot(JuvOut, 
#          params = JuvMon[7],
#          horiz = TRUE, 
#          xlim = c(0, 2))

# Output formatting -----------------------------------------------------------------

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
    par %in% c("lambda", "max_p_juv", "max_surv", "max_prod") ~ levels(lam(sub_pop, sub_pop))[id],
    par == "log_N_int" ~ as.character(id),
    TRUE ~ NA_character_)) %>% 
  left_join(ds %>% select(rowid, year, pop)) %>% 
  select(year, pop, par, method, `2.5%`, `50%`, `97.5%`) %>% 
  mutate(across(year, \(x) x - years(1))) -> JuvOut3

# average rates upon pop, method, & harvests
JuvOut3 %>% 
  filter(par %in% c("p_juv", "productivity", "survival")) %>% 
  group_split(par) %>% 
  map(rowid_to_column, "id") %>% 
  bind_rows() %>% 
  mutate(name = str_c(par, "[", id, "]")) %>% 
  select(year, pop, method, name, par) -> sel

list(
  JuvOut3 %>% 
    filter(par %>% str_starts("max_"), 
           method %>% str_detect("small")) %>% 
    mutate(pop = method %>% str_sub(1, 2) %>% as_factor(), 
           par = case_when(
             par %>% str_detect("juv") ~ "p_juv",
             par %>% str_detect("surv") ~ "survival",
             par %>% str_detect("prod") ~ "productivity"),
           method = "proxy") %>% 
    select(pop, par, method, contains("%")),
  sel %>% 
    filter(method == "counting",
           year(year) >= 2006) %>%
    group_split(pop, par) %>% 
    map(function(x) {
      tmp = as_tibble(JuvOut) %>%
        select(x %>% pull(name)) %>%
        rowMeans()
      tmp_avg = quantile(tmp, c(.025, .5, .975))
      tmp2 = tibble(tmp_avg, name = names(tmp_avg)) %>% 
        pivot_wider(names_from = name, values_from = tmp_avg)
      out = x %>% 
        distinct(pop, par) %>% 
        bind_cols(tmp2) %>% 
        mutate(method = "harvested")
      return(out)
    }) %>% 
    bind_rows()
) %>% 
  bind_rows() %>% 
  mutate(par = str_c(par, "_avg")) -> avg

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
  ungroup() %>% 
  mutate(kind = case_when(
    sub_pop %>% str_detect("small") ~ "under no pressure", 
    !(sub_pop %>% str_detect("pop")) ~ "under high\nharvest pressure", 
    TRUE ~ NA_character_) %>% 
      as_factor()) %>% 
  filter(!is.na(kind)) -> lambda_ds_2


# Vital rate analysis ------------------------------------------------------------------------

# variance explained by harvest strategy
JuvOut4 %>% 
  filter(par %in% c("p_juv", "productivity", "survival"), 
         method == "counting", 
         year(year) >= 2006) %>% 
  group_by(par, pop) %>% 
  mutate(mean_gp = mean(`50%`)) %>% 
  rowwise() %>% 
  mutate(res_gp = `50%` - mean_gp) %>%
  group_by(par) %>% 
  summarize(var_tot = var(`50%`), 
            var_gp = var(res_gp)) %>% 
  mutate(prop_var_exp = str_c(round(1e2 * (var_tot - var_gp) / var_tot), "%"))

# correlation total mortality rate and harvest rate
# without migration and error, points are expected to display above the identity line
counts_1 %>% 
  filter(year(year) <= 2017) %>% 
  left_join(frag_cor) %>%
  mutate(across(shot, ~ replace_na(.x, 0)),
         harvest_rate = (shot / n_win) %>% replace_na(0)) %>% 
  select(year, pop, harvest_rate) %>% 
  right_join(
    JuvOut4 %>% 
      filter(par == "survival") %>% 
      mutate(mortality_rate = 1 -  `50%`) %>% 
      select(year, pop, method, mortality_rate)) -> M_ds
  
M_ds %>% 
  ggplot(aes(x = harvest_rate, y = mortality_rate, color = pop)) +
  geom_abline(slope = 1, intercept = 0) +
  geom_label(aes(label = year(year))) +
  facet_wrap(~ method) +
  lims(x = c(0, 1), y = c(0, 1))

M_ds %>% 
  pivot_longer(contains("rate")) %>% 
  ggplot(aes(x = year, y = value, color = name)) +
  geom_point() +
  geom_line() +
  facet_grid(pop ~ method) +
  ylim(0, 1)

# Output plot -----------------------------------------------------------------------

## Interannual validation ----------------------------------------------------------

JuvOut4 %>% 
  filter(!is.na(method), par %in% c("p_juv", "productivity", "survival")) %>% 
  mutate(par = as_factor(par) %>% fct_relevel(c("p_juv", "survival", "productivity"))) %>% 
  select(year, pop, method, par, `50%`) %>% 
  group_split(par) %>% 
  map(function(x) {
    tmp <- x %>% 
      pivot_wider(names_from = method, values_from = `50%`) %>% 
      filter(!is.na(sampling), !is.na(counting))
    coe <- tls(counting ~ 0 + sampling, data = tmp)$coefficient
    return(tibble(par = unique(x$par), coef = coe))}) %>% 
  bind_rows() %>% 
  mutate(
    title = c("Proportion of immatures in the population", 
              "Survival rate",
              "Recruitment rate"),
    subtitle = c("", 
              "Proportion of breeders still alive after one year",
              "Number of recruits produced per breeder"),
    x_max = c(1, 1, .5),
    x_arrow = c(.75, .70, .4),
    x_txt = c(.75, .65, .4), 
    y_txt = c(.62, .65, .32),
    x_id = .85 * x_max, 
    y_id = .90 * x_max) -> xy 

xy %>% 
  group_split(par) %>%
  map(function(x) {
    tmp = cor_plot(col = x$par, 
                   title = x$title,
                   subtitle = x$subtitle,
                   limits = c(0, x$x_max)) +
      geom_text(x = x$x_id, y = x$y_id, 
                label = "1:1", 
                color = "black",
                hjust = 1, angle = 45) +
      geom_segment(x = x$x_arrow, 
                   y = x$x_arrow, 
                   xend = x$x_arrow, 
                   yend = x$coef * x$x_arrow,
                   arrow = arrow(type = "closed", length = unit(0.3, "cm")),
                   linewidth = .5, color = "#de2d26") +
      geom_text(x = x$x_txt, y = x$y_txt, 
                label = str_c(
                  ifelse(x$coef > 1, " + ", " - "),
                  round(1e2 * abs(x$coef - 1), 1),
                  "% "), 
                color = "#de2d26",
                hjust = 0)
    
    if(x$par == "p_juv") {
      tmp = tmp + 
        theme(
          legend.position = c(.05, .95),
          legend.justification = c("left", "top")) + 
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
        guides(x =  guide_axis(angle = 45))
    } else {
      tmp = tmp + 
        theme(legend.position = "none")
    }
    return(tmp)
  }) -> cor_plots; cor_plots

## Growth rate evolution -----------------------------------------------------------

ds %>% 
  mutate(
    rec = case_when(
      year(year) < 1999 ~ 1,
      year(year) >= 1999 & year(year) < 2006 ~ 2,
      year(year) >= 2006 ~ 3),  
    label = c("no harvest",
              "low harvest",
              "high harvest")[rec] %>% as_factor()) %>% 
  group_by(rec, label) %>% 
  summarize(xmin = if_else(min(year(year)) == 1960, ymd(19500101), min(year)),
            xmax = min(ymd(20210101), max(year) + years(1))) %>% 
  ungroup() %>% 
  select(label, xmin, xmax) %>% 
  mutate(ymin = 10^log10(1e4),
         ymax = 10^(log10(ymin) + 0.15)) -> ts_control

JuvOut3 %>%
  filter(par == "productivity", method != "sampling") %>% 
  group_split(pop, method) %>% 
  map(~ .x %>% 
        mutate(diff = c(1, diff(year(year))) - 1, 
               diff = if_else(diff != 0, 1, 0), 
               diff = cumsum(diff)) %>% 
        group_by(pop, method, diff) %>% 
        summarize(xmin = min(year),
                  xmax = max(year) + years(1)) %>% 
        ungroup()) %>%
  bind_rows() %>% 
  rowid_to_column("sq") %>% 
  select(sq, pop, xmin, xmax) %>% 
  rowwise() %>% 
  mutate(ts = list(seq(xmin, xmax, by = "1 year"))) %>% 
  unnest(ts) %>% 
  select(sq, pop, year = ts) %>% 
  left_join(
    ds %>% 
      select(year, pop, n_win)) -> ts_seq

ds %>% 
  select(year, pop, n_win) %>% 
  anti_join(ts_seq) %>% 
  bind_rows(ts_seq) %>% 
  mutate(across(sq, ~ replace_na(.x, 0)),
         label = if_else(sq != 0, "available\ndichromatic\ndata", "") %>% as_factor() %>% 
           fct_relevel("", after = Inf)) %>% 
  filter(year(year) <= 2019) -> ts_seq2

ggplot() +
  scale_y_log10(minor_breaks = NULL) +
  annotation_logticks(side = "l", color = "grey") +
  scale_color_manual(values = c_pop, 
                     labels = c("GB population", "FR population")) +
  geom_line(data = ts_seq2 %>% arrange(pop, year), 
            aes(x = year, y = n_win, color = pop), linewidth = 1, alpha = .5, linetype = "dotted") +
  geom_line(data = ts_seq2 %>% filter(sq != 0), 
            aes(x = year, y = n_win, color = pop, group = sq), linewidth = 2, alpha = .5) + 
  geom_point(data = ts_seq2, 
             aes(x = year, y = n_win), size = 3, color = "white") +
  geom_point(data = ts_seq2, 
             aes(x = year, y = n_win, color = pop), 
             size = 2, alpha = .5) + 
  geom_point(data = ts_seq2 %>% filter(sq != 0), 
             aes(x = year, y = n_win, color = pop), alpha = 1) +
  scale_x_date(breaks = seq(min(ts_seq2$year), max(ts_seq2$year), "2 years"),
               date_labels = "%Y", 
               expand = c(1e-2, 1e-2)) +
  guides(color = guide_legend(title = "Data availability", order = 1),
         x = guide_axis(angle = 45)) +
  labs(x = NULL, y = NULL) +
  geom_rect(data = ts_control,
            aes(xmin = xmin, xmax = xmax, 
                ymin = ymin, ymax = ymax, alpha = label), 
            fill = "#e6a91d",
            inherit.aes = FALSE) + 
  scale_fill_manual(values = c_pop) + 
  scale_alpha_manual(name = "", values = c(.3, .6, 1)) +
  coord_cartesian(xlim = range(ts_seq2$year) + years(c(-1, 1)),
                  ylim = c(NA, 14000),
                  expand = FALSE) +
  guides(fill = "none",
         alpha = guide_legend(title = "Control pressure", order = 2)) +
  theme(legend.position = "left") -> count_plot; count_plot

ggplot() +
  geom_line(data = lambda_ds_2, 
            aes(x = year, y = exp(y), color = kind, group = sub_ts),
            alpha = .8, linewidth = 6, lineend = "round") +
  scale_color_manual(values = c_met) +
  guides(color = guide_legend(title = "Average pop. growth", order = 3)) + 
  scale_y_log10(minor_breaks = NULL) +
  annotation_logticks(side = "l", color = "grey") +
  ggnewscale::new_scale_color() +
  scale_color_manual(values = c_pop, 
                     labels = c("GB", "FR")) +
  geom_line(data = ts_seq2 %>% arrange(pop, year), 
            aes(x = year, y = n_win, color = pop), linewidth = 1, alpha = 1, linetype = "dotted") +
  geom_point(data = ts_seq2, 
             aes(x = year, y = n_win), size = 3, color = "white") +
  geom_point(data = ts_seq2, 
             aes(x = year, y = n_win, color = pop), 
             size = 2, alpha = 1) + 
  geom_text(data = lambda_ds_2 %>% distinct(sub_pop, lambda, kind), 
            aes(x = c(1972, 1992, 2004, 2016) %>% str_c("0101") %>% ymd(), 
                y = c(500, 50, 1000, 400), 
                label = str_c(if_else(lambda > 1, "+ ", "- "),
                              round(1e2 * abs(lambda - 1)),
                              " %\n per year")),
            color = c_met[2], alpha = 1, size = 4) +
  scale_x_date(breaks = seq(min(ts_seq2$year), max(ts_seq2$year), "2 years"),
               date_labels = "%Y", 
               expand = c(1e-2, 1e-2)) +
  guides(color = guide_legend(title = "Population", order = 1),
         x = guide_axis(angle = 45)) +
  labs(x = NULL, y = NULL) +
  geom_rect(data = ts_control,
            aes(xmin = xmin, xmax = xmax, 
                ymin = ymin, ymax = ymax, alpha = label), 
            fill = "#e6a91d",
            inherit.aes = FALSE) + 
  scale_fill_manual(values = c_pop) + 
  scale_alpha_manual(name = "", values = c(.3, .6, 1)) +
  coord_cartesian(xlim = range(ts_seq2$year) + years(c(-1, 1)),
                  ylim = c(NA, 14000),
                  expand = FALSE) +
  guides(fill = "none",
         alpha = guide_legend(title = "Control pressure", order = 2)) +
  theme(legend.position = "left") -> growth_plot; growth_plot

## Immature proportion and vital rates time series --------------------------

JuvOut4 %>% 
  filter(
    par %in% c("p_juv", "survival", "productivity"), 
    method == "counting") %>% 
  select(pop, year, var = par, val = `50%`) %>% 
  pivot_wider(names_from = var, values_from = val) %>% 
  filter(pop == "FR") %>% 
  select(-c(1:2)) %>% 
  plot()

JuvOut4 %>% 
  filter(
    par %in% c("p_juv", "survival", "productivity"), 
    method == "counting") %>% 
  select(pop, year, var = par, ends_with("%")) %>% 
  mutate(
    year = year + if_else(pop == "GB", months(8), months(5))) -> raw_ds 

JuvOut4 %>% 
  filter(
    par %in% c("p_juv", "survival", "productivity"), 
    method == "counting") %>% 
  select(pop, year, var = par, ends_with("%")) %>% 
  mutate(
    var = as_factor(var) %>% fct_relevel(c("p_juv", "survival", "productivity")),
    year = year + if_else(pop == "GB", months(8), months(5))) %>% 
  group_split(var) %>% 
  imap(~ 
         {tmp = .x %>%
           ggplot(aes(x = year, y = `50%`, color = pop)) +
           geom_pointrange(aes(ymin = `2.5%`, ymax = `97.5%`), 
                           alpha = 0.5, linewidth = 1.5, size = 0.6) + 
           scale_color_manual(values = c_pop) +
           scale_x_date(breaks = str_c(1998:2019, "0101") %>% ymd(),
                        minor_breaks = NULL,
                        date_labels = "%Y") +
           labs(title = c("Proportion of immatures in the population", 
                          "Survival rate",
                          "Recruitment rate")[.y],
                subtitle = c("", 
                             "Proportion of breeders still alive after one year",
                             "Number of recruits produced per breeder")[.y],
                x = NULL, y = NULL)
         
         if(.y == 1) {
           tmp = tmp +
             guides(color = guide_legend(title = "Population", order = 2),
                    x = guide_axis(angle = 45)) +
             geom_rect(data = ts_control %>% 
                         mutate(ymin = 1),
                       aes(xmin = xmin, xmax = xmax, 
                           ymin = ymin, ymax = ymax, alpha = label), 
                       fill = "#e6a91d",
                       inherit.aes = FALSE) + 
             scale_fill_manual(values = c_pop) + 
             scale_alpha_manual(name = "", values = c(.3, .6, 1)) +
             coord_cartesian(xlim = range(.x$year) + years(c(-1, 1)),
                             ylim = c(-0.05, 1.05),
                             expand = FALSE) + 
             guides(fill = "none",
                    alpha = guide_legend(title = "Control pressure", order = 1)) +
             scale_y_continuous(
               labels = scales::percent_format(accuracy = 1L),
               breaks = 0:10 / 10, 
               minor_breaks = NULL) +
             theme(
               legend.position = c(.05, .90),
               legend.justification = c("left", "top"),
               legend.direction = "horizontal",
               legend.key.size = unit(0.5, "cm"))
         } else {
           tmp = tmp +
             guides(x = guide_axis(angle = 45)) +
             geom_rect(data = ts_control %>% 
                         mutate(ymin = 1.143),
                       aes(xmin = xmin, xmax = xmax, 
                           ymin = ymin, ymax = ymax, alpha = label), 
                       fill = "#e6a91d",
                       inherit.aes = FALSE) + 
             scale_fill_manual(values = c_pop) + 
             scale_alpha_manual(name = "", values = c(.3, .6, 1)) +
             coord_cartesian(xlim = range(.x$year) + years(c(-1, 1)),
                             ylim = c(-0.05, 1.2),
                             expand = FALSE) + 
             theme(legend.position = "none") + 
             scale_y_continuous(
             breaks = 0:11 / 10,
             minor_breaks = NULL)
         } 
         return(tmp)}) -> rate_plots; rate_plots

## Comparison of harvested vs max growth ------------------------------

JuvOut4 %>% 
  mutate(par = if_else(par == "lambda" & !str_detect(method, "large"), "lambda_avg", par),
         pop = if_else(par == "lambda_avg", method %>% str_sub(1, 2) %>% as_factor(), pop)) %>% 
  filter(par %>% str_ends("_avg"), !is.na(pop)) %>% 
  mutate(box = case_when(
    method %>% str_detect("proxy|small") ~ "...under no pressure",
    TRUE ~ "...under high\nharvest pressure") %>% 
      as_factor() %>% 
      fct_relevel("...under high\nharvest pressure", after = Inf), 
    across(par, ~ .x %>% as_factor() %>% fct_relevel("productivity_avg", after = Inf)),
    across(contains("%"), ~ if_else(method %>% str_detect("sampl"), NA_real_, .x))) %>% 
  select(-year, -method) %>% 
  arrange(pop, box) -> avg_data

avg_data %>% 
  group_split(par) %>% 
  imap( ~ {
    tmp = .x %>% 
      ggplot(aes(x = pop, y = `50%`, color = pop)) +
      facet_wrap(~ box, nrow = 1) +
      theme(strip.text.x = element_text(size = 12)) +
      geom_pointrange(aes(ymin = `2.5%`, ymax = `97.5%`), 
                      alpha = 0.5, linewidth = 1.5, size = 0.2) +
      # geom_point(size = 1.5) +
      scale_color_manual(values = c_pop, guide = "none") +
      labs(title = c("Average pop. growth...",
                     "Average immature proportion...",
                     "Average survival rate...",
                     "Average recruitment rate...")[.y],
           x = NULL, y = NULL)
    
    if(.y == 1) {
      tmp = tmp + scale_y_continuous(
        limits = c(0, 2),
        breaks = 0:20 / 10,
        minor_breaks = NULL)
    } else if(.y == 2) {
      tmp = tmp + scale_y_percent()
    } else {
      tmp = tmp + scale_y_continuous(
        limits = c(0, 1.15),
        breaks = 0:11 / 10,
        minor_breaks = NULL)
    }
    
    return(tmp)}) -> avg_plots; avg_plots


full_join(
  avg_data %>% 
    filter(box %>% str_detect("no")) %>% 
    select(pop, par, no_harv = `50%`),
  avg_data %>% 
    filter(!box %>% str_detect("no")) %>% 
    select(pop, par, harv = `50%`)) %>% 
  mutate(drop = no_harv - harv)

# Layout & save -----------------------------------------------------------------------

intro_plot
count_plot

grid.arrange(
  grobs = rate_plots,
  widths = 5,
  heights = rep(4, 3),
  layout_matrix = cbind(c(1, 2, 3))) -> rate_plot

grid.arrange(
  grobs = cor_plots,
  widths = 5,
  heights = rep(5, 3),
  layout_matrix = cbind(c(1, 2, 3))) -> cor_plot

hlay <- rbind(c(1, 1, 1, NA),
              c(NA, NA, NA, NA),
              c(NA, 2, 2, NA),
              c(NA, NA, NA, NA),
              c(3, 3, 4, 4))

grid.arrange(
  grobs = c(list(growth_plot), avg_plots[-2]),
  widths = rep(5, 4),
  heights = c(5.5, 1, 5, 1, 5),
  layout_matrix = hlay) -> strat_plot

harv_plot

list(intro_plot,
     count_plot, 
     rate_plot, 
     cor_plot,
     strat_plot,
     harv_plot) -> plo_paper

list(c(8, 3), 
     c(9, 5),
     5 * c(1, 3),
     5 * c(1, 3),
     11 * c(1, 1), 
     4 * c(2, 1)) -> plo_dim_paper

list(plo_paper, c(1:length(plo_paper)), plo_dim_paper) %>% 
  pwalk(~ ggsave(filename = str_c("plot_paper_", ..2, ".png"),
                 path = "./Output",
                 plot = ..1, 
                 width = ..3[1], 
                 height = ..3[2], dpi = 600))

