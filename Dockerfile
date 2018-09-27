FROM heroku/heroku:18

ENV LANG C.UTF-8

# Install required packages.
RUN apt-get update
RUN apt-get upgrade -y --assume-yes
# Install packages for stack and ghc.
RUN apt-get install -y --assume-yes xz-utils gcc libgmp-dev zlib1g-dev
# Install packages needed for libraries used by our app.
RUN apt-get install -y --assume-yes libpq-dev
# Install convenience utilities, like tree, ping, and vim.
RUN apt-get install -y --assume-yes tree iputils-ping vim-nox

# Recently added
RUN apt-get install -y --assume-yes pkg-config libcairo2-dev
RUN apt-get install -y --assume-yes libssl-dev libcurl4-gnutls-dev  
RUN apt-get install -y --assume-yes libc6

# Remove apt caches to reduce the size of our container.
RUN rm -rf /var/lib/apt/lists/*

# Install stack to /opt/stack/bin.
RUN mkdir -p /opt/stack/bin
RUN curl -L https://www.stackage.org/stack/linux-x86_64 | tar xz --wildcards --strip-components=1 -C /opt/stack/bin '*/stack'

# Create /opt/fs6/bin and /opt/fs6/src.  Set
# /opt/fs6/src as the working directory.
RUN mkdir -p /opt/fs6/src
RUN mkdir -p /opt/fs6/bin
WORKDIR /opt/fs6/src

# Set the PATH for the root user so they can use stack.
ENV PATH "$PATH:/opt/stack/bin:/opt/fs6/bin"

# Install GHC using stack, based on your app's stack.yaml file.
COPY ./stack.yaml /opt/fs6/src/stack.yaml
RUN stack --no-terminal setup

# Install all dependencies in app's .cabal file.
COPY ./fs6.cabal /opt/fs6/src/fs6.cabal
# RUN stack install servant-0.10
# RUN stack install servant-server-0.10
RUN stack --no-terminal test --only-dependencies

# Build application.
COPY . /opt/fs6/src
RUN stack -v --no-terminal build

# Install application binaries to /opt/fs6/bin.
RUN stack --no-terminal --local-bin-path /opt/fs6/bin install

# Remove source code.
RUN mkdir -p /opt/fs6/config
COPY ./config/config.ini /opt/fs6/config/config.ini
#RUN rm -rf /opt/fs6/src

# Add the apiuser and setup their PATH.
RUN useradd -ms /bin/bash apiuser
RUN chown -R apiuser:apiuser /opt/fs6
USER apiuser
ENV PATH "$PATH:/opt/stack/bin:/opt/fs6/bin"

# Set the working directory as /opt/fs6/.
WORKDIR /opt/fs6

CMD /opt/fs6/bin/file-processor
