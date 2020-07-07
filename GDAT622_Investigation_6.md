GDAT622 Investigation 6 JES
================
Jessica Smyrski
7/2/2020

``` r
date <- Sys.Date()

app.name <- "Jess's App"
Customer.Key <- "xIHTKdlmcaLyJaMqCy3DzaBcy"
Customer.Secret <- "zTRV9RgUJTvjR425PnhHd1mRXnHETmVaXL1kX6DhJ6CaHoIocW"

token <- create_token(app = app.name,
                      consumer_key = Customer.Key,
                      consumer_secret = Customer.Secret)
```

I wanted to look at Popeyes Twitter account. Considering their new
chicken sandwich was such a hit, I was curious to see if they were
influenced by other Fast Food chains.

``` r
Popeyes_Follow <- get_followers("PopeyesChicken")
Popeyes_Friends <- get_friends("PopeyesChicken")
glimpse(Popeyes_Follow)
```

    ## Observations: 5,000
    ## Variables: 1
    ## $ user_id <chr> "1249054314968080386", "1122687989438713856", "9149371...

``` r
Pop_Sample <- sample(Popeyes_Follow$user_id, size = 75)
```

``` r
#referenced from: https://rpubs.com/ben_bellman/rtweet_tidygraph

# create empty list to store results
friends <- list()
# start loop
for (a in 1:length(Pop_Sample)){
  friends[[a]] <- get_friends(Pop_Sample[a])
  
  # pause if divisible by 15
  if (a %% 15 == 0){
    Sys.sleep(15*60) # must enter time in seconds
  }
}
# Combine data tables in list
friends <- bind_rows(friends) %>% 
  rename(friend = user_id)
```

``` r
dplyr::filter(friends, Popeyes_Follow %in% friend)
```

    ## # A tibble: 0 x 2
    ## # ... with 2 variables: user <chr>, friend <chr>

``` r
net <- friends %>% 
  group_by(friend) %>% 
  mutate(count = n()) %>% 
  ungroup()
g <- graph_from_data_frame(net)

# Plotting the Networks

plot(g,
     edge.arrow.size = 0.35,
     vertex.label.cex = 0.75)
```

![](GDAT622_Investigation_6_files/figure-gfm/Plotting%20the%20Data-1.png)<!-- -->

``` r
lookup_users(274627449)
```

    ## # A tibble: 1 x 90
    ##   user_id status_id created_at screen_name text  source display_text_wi~
    ##   <chr>   <chr>     <chr>      <chr>       <chr> <chr>             <int>
    ## 1 274627~ 274627449 <NA>       andreacs13  <NA>  <NA>                 NA
    ## # ... with 83 more variables: reply_to_status_id <chr>,
    ## #   reply_to_user_id <chr>, reply_to_screen_name <chr>, is_quote <lgl>,
    ## #   is_retweet <lgl>, favorite_count <int>, retweet_count <int>,
    ## #   quote_count <int>, reply_count <int>, hashtags <list>, symbols <list>,
    ## #   urls_url <list>, urls_t.co <list>, urls_expanded_url <list>,
    ## #   media_url <list>, media_t.co <list>, media_expanded_url <list>,
    ## #   media_type <list>, ext_media_url <list>, ext_media_t.co <list>,
    ## #   ext_media_expanded_url <list>, ext_media_type <chr>,
    ## #   mentions_user_id <list>, mentions_screen_name <list>, lang <chr>,
    ## #   quoted_status_id <chr>, quoted_text <chr>, quoted_created_at <dttm>,
    ## #   quoted_source <chr>, quoted_favorite_count <int>,
    ## #   quoted_retweet_count <int>, quoted_user_id <chr>,
    ## #   quoted_screen_name <chr>, quoted_name <chr>,
    ## #   quoted_followers_count <int>, quoted_friends_count <int>,
    ## #   quoted_statuses_count <int>, quoted_location <chr>,
    ## #   quoted_description <chr>, quoted_verified <lgl>,
    ## #   retweet_status_id <chr>, retweet_text <chr>,
    ## #   retweet_created_at <dttm>, retweet_source <chr>,
    ## #   retweet_favorite_count <int>, retweet_retweet_count <int>,
    ## #   retweet_user_id <chr>, retweet_screen_name <chr>, retweet_name <chr>,
    ## #   retweet_followers_count <int>, retweet_friends_count <int>,
    ## #   retweet_statuses_count <int>, retweet_location <chr>,
    ## #   retweet_description <chr>, retweet_verified <lgl>, place_url <chr>,
    ## #   place_name <chr>, place_full_name <chr>, place_type <chr>,
    ## #   country <chr>, country_code <chr>, geo_coords <list>,
    ## #   coords_coords <list>, bbox_coords <list>, status_url <chr>,
    ## #   name <chr>, location <chr>, description <chr>, url <lgl>,
    ## #   protected <lgl>, followers_count <int>, friends_count <int>,
    ## #   listed_count <int>, statuses_count <int>, favourites_count <int>,
    ## #   account_created_at <dttm>, verified <lgl>, profile_url <chr>,
    ## #   profile_expanded_url <chr>, account_lang <lgl>,
    ## #   profile_banner_url <lgl>, profile_background_url <chr>,
    ## #   profile_image_url <chr>

``` r
Popeyes_Common <- Popeyes_Friends %>% 
  select(user_id) %>% 
  mutate(follow = TRUE) %>% 
  full_join(Popeyes_Follow %>% mutate(follows_popeyes = TRUE),
            by = "user_id") %>% 
  mutate_all(funs(replace(., which(is.na(.)), FALSE))) %>% 
  select(1:3)

dplyr::filter(Popeyes_Common, follow == TRUE, follows_popeyes == TRUE) #3 people have mutual connection to the company
```

    ## # A tibble: 3 x 3
    ##   user_id  follow follows_popeyes
    ##   <chr>    <lgl>  <lgl>          
    ## 1 20605481 TRUE   TRUE           
    ## 2 78575495 TRUE   TRUE           
    ## 3 23637129 TRUE   TRUE

``` r
lookup_users("20605481") #@AdrianChen
```

    ## # A tibble: 1 x 90
    ##   user_id status_id created_at          screen_name text  source
    ##   <chr>   <chr>     <dttm>              <chr>       <chr> <chr> 
    ## 1 206054~ 12802154~ 2020-07-06 19:02:04 AdrianChen  @scr~ Twitt~
    ## # ... with 84 more variables: display_text_width <int>,
    ## #   reply_to_status_id <chr>, reply_to_user_id <chr>,
    ## #   reply_to_screen_name <chr>, is_quote <lgl>, is_retweet <lgl>,
    ## #   favorite_count <int>, retweet_count <int>, quote_count <int>,
    ## #   reply_count <int>, hashtags <list>, symbols <list>, urls_url <list>,
    ## #   urls_t.co <list>, urls_expanded_url <list>, media_url <list>,
    ## #   media_t.co <list>, media_expanded_url <list>, media_type <list>,
    ## #   ext_media_url <list>, ext_media_t.co <list>,
    ## #   ext_media_expanded_url <list>, ext_media_type <chr>,
    ## #   mentions_user_id <list>, mentions_screen_name <list>, lang <chr>,
    ## #   quoted_status_id <chr>, quoted_text <chr>, quoted_created_at <dttm>,
    ## #   quoted_source <chr>, quoted_favorite_count <int>,
    ## #   quoted_retweet_count <int>, quoted_user_id <chr>,
    ## #   quoted_screen_name <chr>, quoted_name <chr>,
    ## #   quoted_followers_count <int>, quoted_friends_count <int>,
    ## #   quoted_statuses_count <int>, quoted_location <chr>,
    ## #   quoted_description <chr>, quoted_verified <lgl>,
    ## #   retweet_status_id <chr>, retweet_text <chr>,
    ## #   retweet_created_at <dttm>, retweet_source <chr>,
    ## #   retweet_favorite_count <int>, retweet_retweet_count <int>,
    ## #   retweet_user_id <chr>, retweet_screen_name <chr>, retweet_name <chr>,
    ## #   retweet_followers_count <int>, retweet_friends_count <int>,
    ## #   retweet_statuses_count <int>, retweet_location <chr>,
    ## #   retweet_description <chr>, retweet_verified <lgl>, place_url <chr>,
    ## #   place_name <chr>, place_full_name <chr>, place_type <chr>,
    ## #   country <chr>, country_code <chr>, geo_coords <list>,
    ## #   coords_coords <list>, bbox_coords <list>, status_url <chr>,
    ## #   name <chr>, location <chr>, description <chr>, url <chr>,
    ## #   protected <lgl>, followers_count <int>, friends_count <int>,
    ## #   listed_count <int>, statuses_count <int>, favourites_count <int>,
    ## #   account_created_at <dttm>, verified <lgl>, profile_url <chr>,
    ## #   profile_expanded_url <chr>, account_lang <lgl>,
    ## #   profile_banner_url <chr>, profile_background_url <chr>,
    ## #   profile_image_url <chr>

``` r
lookup_users("23637129") #@OCRashad
```

    ## # A tibble: 1 x 90
    ##   user_id status_id created_at          screen_name text  source
    ##   <chr>   <chr>     <dttm>              <chr>       <chr> <chr> 
    ## 1 236371~ 12799835~ 2020-07-06 03:40:34 OCRashad    @Veg~ Twitt~
    ## # ... with 84 more variables: display_text_width <int>,
    ## #   reply_to_status_id <chr>, reply_to_user_id <chr>,
    ## #   reply_to_screen_name <chr>, is_quote <lgl>, is_retweet <lgl>,
    ## #   favorite_count <int>, retweet_count <int>, quote_count <int>,
    ## #   reply_count <int>, hashtags <list>, symbols <list>, urls_url <list>,
    ## #   urls_t.co <list>, urls_expanded_url <list>, media_url <list>,
    ## #   media_t.co <list>, media_expanded_url <list>, media_type <list>,
    ## #   ext_media_url <list>, ext_media_t.co <list>,
    ## #   ext_media_expanded_url <list>, ext_media_type <chr>,
    ## #   mentions_user_id <list>, mentions_screen_name <list>, lang <chr>,
    ## #   quoted_status_id <chr>, quoted_text <chr>, quoted_created_at <dttm>,
    ## #   quoted_source <chr>, quoted_favorite_count <int>,
    ## #   quoted_retweet_count <int>, quoted_user_id <chr>,
    ## #   quoted_screen_name <chr>, quoted_name <chr>,
    ## #   quoted_followers_count <int>, quoted_friends_count <int>,
    ## #   quoted_statuses_count <int>, quoted_location <chr>,
    ## #   quoted_description <chr>, quoted_verified <lgl>,
    ## #   retweet_status_id <chr>, retweet_text <chr>,
    ## #   retweet_created_at <dttm>, retweet_source <chr>,
    ## #   retweet_favorite_count <int>, retweet_retweet_count <int>,
    ## #   retweet_user_id <chr>, retweet_screen_name <chr>, retweet_name <chr>,
    ## #   retweet_followers_count <int>, retweet_friends_count <int>,
    ## #   retweet_statuses_count <int>, retweet_location <chr>,
    ## #   retweet_description <chr>, retweet_verified <lgl>, place_url <chr>,
    ## #   place_name <chr>, place_full_name <chr>, place_type <chr>,
    ## #   country <chr>, country_code <chr>, geo_coords <list>,
    ## #   coords_coords <list>, bbox_coords <list>, status_url <chr>,
    ## #   name <chr>, location <chr>, description <chr>, url <lgl>,
    ## #   protected <lgl>, followers_count <int>, friends_count <int>,
    ## #   listed_count <int>, statuses_count <int>, favourites_count <int>,
    ## #   account_created_at <dttm>, verified <lgl>, profile_url <chr>,
    ## #   profile_expanded_url <chr>, account_lang <lgl>,
    ## #   profile_banner_url <lgl>, profile_background_url <chr>,
    ## #   profile_image_url <chr>

``` r
lookup_users("78575495") #@steakshapiro
```

    ## # A tibble: 1 x 90
    ##   user_id status_id created_at          screen_name text  source
    ##   <chr>   <chr>     <dttm>              <chr>       <chr> <chr> 
    ## 1 785754~ 12803299~ 2020-07-07 02:37:10 steakshapi~ Grea~ Twitt~
    ## # ... with 84 more variables: display_text_width <int>,
    ## #   reply_to_status_id <lgl>, reply_to_user_id <lgl>,
    ## #   reply_to_screen_name <lgl>, is_quote <lgl>, is_retweet <lgl>,
    ## #   favorite_count <int>, retweet_count <int>, quote_count <int>,
    ## #   reply_count <int>, hashtags <list>, symbols <list>, urls_url <list>,
    ## #   urls_t.co <list>, urls_expanded_url <list>, media_url <list>,
    ## #   media_t.co <list>, media_expanded_url <list>, media_type <list>,
    ## #   ext_media_url <list>, ext_media_t.co <list>,
    ## #   ext_media_expanded_url <list>, ext_media_type <chr>,
    ## #   mentions_user_id <list>, mentions_screen_name <list>, lang <chr>,
    ## #   quoted_status_id <chr>, quoted_text <chr>, quoted_created_at <dttm>,
    ## #   quoted_source <chr>, quoted_favorite_count <int>,
    ## #   quoted_retweet_count <int>, quoted_user_id <chr>,
    ## #   quoted_screen_name <chr>, quoted_name <chr>,
    ## #   quoted_followers_count <int>, quoted_friends_count <int>,
    ## #   quoted_statuses_count <int>, quoted_location <chr>,
    ## #   quoted_description <chr>, quoted_verified <lgl>,
    ## #   retweet_status_id <chr>, retweet_text <chr>,
    ## #   retweet_created_at <dttm>, retweet_source <chr>,
    ## #   retweet_favorite_count <int>, retweet_retweet_count <int>,
    ## #   retweet_user_id <chr>, retweet_screen_name <chr>, retweet_name <chr>,
    ## #   retweet_followers_count <int>, retweet_friends_count <int>,
    ## #   retweet_statuses_count <int>, retweet_location <chr>,
    ## #   retweet_description <chr>, retweet_verified <lgl>, place_url <chr>,
    ## #   place_name <chr>, place_full_name <chr>, place_type <chr>,
    ## #   country <chr>, country_code <chr>, geo_coords <list>,
    ## #   coords_coords <list>, bbox_coords <list>, status_url <chr>,
    ## #   name <chr>, location <chr>, description <chr>, url <chr>,
    ## #   protected <lgl>, followers_count <int>, friends_count <int>,
    ## #   listed_count <int>, statuses_count <int>, favourites_count <int>,
    ## #   account_created_at <dttm>, verified <lgl>, profile_url <chr>,
    ## #   profile_expanded_url <chr>, account_lang <lgl>,
    ## #   profile_banner_url <lgl>, profile_background_url <chr>,
    ## #   profile_image_url <chr>

I was surprised to see that no fast food accounts had mutual
connections. I was expecting to see Popeyes following other major fast
food accounts and they would be following Popeyes account as well. It
looks like the three people who follow Popeyes, that Popeyes also
follows is pretty random. If I had to guess I would say that Popeyes
decided to follow random accounts who were following them.

``` r
get_friends(20605481) -> Following_1
get_friends(23637129) -> Following_2
get_friends(78575495) -> Following_3

Following_Combined <- bind_rows(Popeyes_Follow, Following_1, Following_2, Following_3) 
```

``` r
Following_g <- graph.data.frame(Following_Combined)
plot(Following_g)
```

![](GDAT622_Investigation_6_files/figure-gfm/Friends%20of%20Followers%20Graph-1.png)<!-- -->
It’s hard to visualize the plot above, but there are definitely mutual
followers between the three accounts and Popeyes. Not really sure what
this tells us, other than Popeyes follows a lot of verified or “famous”
individuals. Whether those individuals influence the company to create
different foods is another unknown.
