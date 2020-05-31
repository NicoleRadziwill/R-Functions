## Regular Expressions are strings that match conditions. Here, we set up three:
##   * =re.url= to verify properly formatted URLs
##   * =re.email= to verify properly formatted email addresses
##   * =re.currency= to detect dollars, pounds, and euros

library(rex)

valid_chars <- rex(except_some_of(".", "/", " ", "-"))
re.url <- rex(
  start,
  # Conditions for protocols at the beginning of the URL, if it exists. Swap commenting if protocol required.
         maybe(list("http", maybe("s")) %or% "ftp", "://"),  # HTTP/FTP prefix not required in a URL
     #   group(list("http", maybe("s")) %or% "ftp", "://"),  # HTTP/FPT prefix required in a URL
  # Conditions for user:pass authentication passed in the URL, if it exists
     maybe(non_spaces, maybe(":", zero_or_more(non_space)), "@"),
  # Conditions for host names
     group(zero_or_more(valid_chars, zero_or_more("-")), one_or_more(valid_chars)),
  # Conditions for domain names
     zero_or_more(".", zero_or_more(valid_chars, zero_or_more("-")), one_or_more(valid_chars)),
  # Conditions for top-level domains (e.g. .com, .co, .in)
     group(".", valid_chars %>% at_least(2)),
  # Conditions for server port numbers, if they exist
     maybe(":", digit %>% between(2, 5)),
  # Conditions for APIs/endpoint identifiers, if they exist
     maybe("/", non_space %>% zero_or_more()),
     end
)

re.email <- "\\<[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,}\\>"
re.currency <- "^([£€$]([0-9]([0-9,])*)(.\\d{2})?|([0-9]([0-9,]))(.\\d{2})?([pcmMK]|bn| [mb]illion))$"
