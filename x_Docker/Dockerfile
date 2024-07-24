# ベースイメージを指定
FROM rocker/r-ver:4.1.0

# Node.jsとnpmをインストール
RUN apt-get update && apt-get install -y nodejs npm

# 必要なRパッケージをインストール
RUN R -e "install.packages(c('V8', 'devtools'))"
RUN R -e "devtools::install_github('hongyuanjia/eplusr')"
RUN R -e "devtools::install_github('hongyuanjia/epluspar')"

# 作業ディレクトリを設定
WORKDIR /usr/local/src/myscripts

#ポートを公開
EXPOSE 8787

# RStudio Serverを起動
CMD ["/init"]