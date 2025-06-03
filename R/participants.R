library(dplyr)

participants <- googlesheets4::read_sheet("1B2HtesFc3gP9WiVHvm33aJVX4UkFMYaUTisFFrsMJtE") |> 
  slice(-10)

participants

library(httr2)

gh_profile_req <- httr2::request("https://api.github.com/users/") |> 
  req_auth_bearer_token(gh::gh_token())
get_github_profile <- function(username) {
  req <- req_url_path(gh_profile_req, path = file.path("users", username))
  resp <- req_perform(req)
  resp_body_json(resp)
}

gh_profiles <- participants$`GitHub username` |> purrr::map_dfr(get_github_profile)

gh_profiles$name

participants |> 
  bind_cols(gh_profiles) |> 
  arrange(desc(followers)) |> 
  rowwise() |> 
  transmute(
    name = if_else(is.na(name), `Full name`, name),
    affiliation = `Affiliated organization`,
    thumbnail = avatar_url,
    social = list(list(list(
      github = html_url
    ))),
    other = `Please share any other social media handles for the workshop website.`
  ) |> 
  purrr:::transpose() |> 
  yaml::as.yaml() |> 
  cat()
