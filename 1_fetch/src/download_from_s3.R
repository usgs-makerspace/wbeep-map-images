download_s3_file <- function(target_name, s3_bucket, s3_file_loc, s3_filename) {
  
  # Setup credentials
  aws.signature::use_credentials(profile='default', file=aws.signature::default_credentials_file())
  
  # Download data
  aws.s3::save_object(object = file.path(s3_file_loc, s3_filename), 
                      bucket = s3_bucket,
                      file = target_name)
}
