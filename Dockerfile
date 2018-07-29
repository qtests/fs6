FROM heroku/heroku:16

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

# Remove apt caches to reduce the size of our container.
RUN rm -rf /var/lib/apt/lists/*

# Install stack to /opt/stack/bin.
RUN mkdir -p /opt/stack/bin
RUN curl -L https://www.stackage.org/stack/linux-x86_64 | tar xz --wildcards --strip-components=1 -C /opt/stack/bin '*/stack'

# Create /opt/fs5/bin and /opt/fs5/src.  Set
# /opt/fs5/src as the working directory.
RUN mkdir -p /opt/fs5/src
RUN mkdir -p /opt/fs5/bin
WORKDIR /opt/fs5/src

# Set the PATH for the root user so they can use stack.
ENV PATH "$PATH:/opt/stack/bin:/opt/fs5/bin"

# Install GHC using stack, based on your app's stack.yaml file.
COPY ./stack.yaml /opt/fs5/src/stack.yaml
RUN stack --no-terminal setup

# Install all dependencies in app's .cabal file.
COPY ./fs5.cabal /opt/fs5/src/fs5.cabal
# RUN stack install servant-0.10
# RUN stack install servant-server-0.10
RUN stack --no-terminal test --only-dependencies

# Build application.
COPY . /opt/fs5/src
RUN stack -v --no-terminal build

# Install application binaries to /opt/fs5/bin.
RUN stack --no-terminal --local-bin-path /opt/fs5/bin install

# Remove source code.
#RUN rm -rf /opt/fs5/src

# Add the apiuser and setup their PATH.
RUN useradd -ms /bin/bash apiuser
RUN chown -R apiuser:apiuser /opt/fs5
USER apiuser
ENV PATH "$PATH:/opt/stack/bin:/opt/fs5/bin"

# Set the working directory as /opt/fs5/.
WORKDIR /opt/fs5

CMD /opt/fs5/bin/file-processor
