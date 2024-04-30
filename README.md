# Installation

- Go to [https://gitlab.uzh.ch/-/user_settings/personal_access_tokens](https://gitlab.uzh.ch/-/user_settings/personal_access_tokens) and generate a personal access token if you do not have one already

In an R session 
```R
install.packages(install.packages(c("devtools", "keyring")))

keyring::key_set("gitlab.uzh")
```

Paste your personal access token into the "password" field when prompted. Then:

```R
devtools::install_gitlab("jaschamax.egli/egstras", host = "gitlab.uzh.ch", auth_token = keyring::key_get("gitlab.uzh"))
```



