# iamrpt: Convert GCAM output to the format used by public IAM databases

Provide functions for converting GCAM output into the format used by
most IAM experiments to enter results into their databases.  Users
provide a table of desired outputs, along with options (such as
filtering and aggregation), and the package runs the necessary GCAM
queries (no more than once per query) and passes the results to the
functions that produce the output.
