FROM haskell:7.8
MAINTAINER Steven McCarthy <steve@redlua.com>

RUN cabal update

RUN mkdir /opt/onetimefiles-com

ADD ./onetimefiles-com.cabal /opt/onetimefiles-com/

RUN cd /opt/onetimefiles-com && cabal sandbox init && cabal install --only-dependencies -j4


ADD . /opt/onetimefiles-com
WORKDIR /opt/onetimefiles-com

RUN cabal install
RUN cabal build

ENV PATH /root/.cabal/bin:$PATH


CMD ["/opt/onetimefiles-com/dist/build/onetimefiles-com/onetimefiles-com"]

