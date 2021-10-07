library(piggyback)
pb_upload(file="data/all_1819.rds",repo = "williamsbenjamin/nflsimulator_aoor",.token = "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")
pb_upload(file="data/fourth_down_sims.rds",repo = "williamsbenjamin/nflsimulator_aoor",.token = "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")
pb_upload(file="data/pbp-data-1819.rds",repo = "williamsbenjamin/nflsimulator_aoor",.token = "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")
pb_upload(file="data/playoffs_sims.rds",repo = "williamsbenjamin/nflsimulator_aoor",.token = "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")
pb_upload(file="data/QBR_thirds_sims.rds",repo = "williamsbenjamin/nflsimulator_aoor",.token = "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")
pb_upload(file="data/yds_less_than_sims.rds",repo = "williamsbenjamin/nflsimulator_aoor",.token = "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")

usethis::use_git_config(user.name = "williamsbenjamin", user.email = "benjamin.williams@du.edu")

## create a personal access token for authentication:
usethis::create_github_token() 
## in case usethis version < 2.0.0: usethis::browse_github_token() (or even better: update usethis!)
## set personal access token:
credentials::set_github_pat("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")
