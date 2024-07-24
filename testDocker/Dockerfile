# ベースイメージを指定
FROM rocker/r-ver:4.1.0

#ポートを公開
EXPOSE 8787

# RStudio Serverを起動
CMD ["/usr/lib/rstudio-server/bin/rserver", "--server-daemonize", "0"]