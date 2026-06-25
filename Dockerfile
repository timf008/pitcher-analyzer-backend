FROM ubuntu:22.04

# Prevent tzdata and other packages from prompting for input
ENV DEBIAN_FRONTEND=noninteractive

# Install system dependencies
RUN apt-get update && \
    apt-get install -y software-properties-common curl gnupg && \
    apt-get install -y r-base r-base-dev

# Install Node.js 18 (Render-compatible)
RUN curl -fsSL https://deb.nodesource.com/setup_18.x | bash - && \
    apt-get install -y nodejs

# Install R packages
RUN R -e "install.packages(c('readr','dplyr','jsonlite','stringr'), repos='https://cloud.r-project.org')"

# Set working directory
WORKDIR /app

# Copy package files
COPY package*.json ./

# Install Node dependencies
RUN npm install

# Copy the rest of the app
COPY . .

# Expose port (Render uses $PORT)
EXPOSE 3000

# Start the server
CMD ["npm", "start"]


