FROM ubuntu:22.04

# Install system dependencies
RUN apt-get update && \
    apt-get install -y r-base r-base-dev \
    r-cran-readr r-cran-dplyr r-cran-jsonlite r-cran-stringr \
    nodejs npm

# Set working directory
WORKDIR /app

# Copy package files
COPY package*.json ./

# Install Node dependencies
RUN npm install

# Copy the rest of the app
COPY . .

# Expose port
EXPOSE 3000

# Start the server
CMD ["npm", "start"]
