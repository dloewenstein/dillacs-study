FROM continuumio/miniconda3

WORKDIR /project

RUN apt-get update && apt-get install -y \
    make \
    wget \
    unzip \
    tar \
    fontconfig \
    perl \
    libgetopt-long-descriptive-perl \
    libdigest-perl-md5-perl \
    libncurses5 && \
    rm -rf /var/lib/apt/lists/*

COPY env.yml .
RUN conda env create -f env.yml

RUN wget http://mirror.ctan.org/systems/texlive/tlnet/install-tl-unx.tar.gz && \
    tar xzf install-tl-unx.tar.gz && rm install-tl-unx.tar.gz && \
    cd install-tl* && \
    echo "selected_scheme scheme-basic" > install.profile && \
    echo "tlpdbopt_install_docfiles 0" >> install.profile && \
    echo "tlpdbopt_install_srcfiles 0" >> install.profile && \
    echo "tlpdbopt_autobackup 0" >> install.profile && \
    echo "tlpdbopt_sys_bin /usr/bin" >> install.profile && \
    perl ./install-tl -profile install.profile && cd .. && rm -rf install-tl*

RUN /usr/local/texlive/2022/bin/x86_64-linux/tlmgr path add

RUN tlmgr install xcolor booktabs etoolbox mdwtools relsize elsarticle lineno placeins

SHELL ["conda", "run", "-n", "dillacs", "/bin/bash", "-c"]

COPY Makefile .

CMD ["make", "all"]
