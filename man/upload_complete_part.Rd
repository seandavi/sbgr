% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/1.upload.R
\name{upload_complete_part}
\alias{upload_complete_part}
\title{Reports the completion of the part upload}
\usage{
upload_complete_part(auth_token = NULL, upload_id = NULL,
  part_number = NULL, e_tag = NULL, ...)
}
\arguments{
\item{auth_token}{auth token}

\item{upload_id}{ID of the upload}

\item{part_number}{ID of the part you wish to report as completed}

\item{e_tag}{Value of the ETag header returned by AWS S3 when uploading
part of the file.}

\item{...}{parameters passed to sbgapi function}
}
\value{
parsed list of the returned json
}
\description{
The ETag is provided for the correctness check upon completion of the
whole upload. Value for the ETag is provided by AWS S3 service when
uploading the file in the ETag header.
}
\examples{
token = '58aeb140-1970-0130-6386-001f5b34aa78'
\donttest{req = upload_complete_part(token,
                upload_id = '8D7sQJxQk14ubsEnKaoeQZlRvV6ouQtMzBWaQNJdxPDLypUC3WogwtJdncevHxnT',
                part_number = '1',
                e_tag = 'd41d8cd98f00b204e9800998ecf8427e')}
}

