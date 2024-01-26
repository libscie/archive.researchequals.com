#!/bin/bash

# Set your GitHub token and username
TOKEN="YOUR_GITHUB_TOKEN"
USERNAME="YOUR_GITHUB_USERNAME"

# Create a CSV file and write the headers
echo "Repo Name,Title,Comments,URL,Date Created,Date Closed,State,Type,Days to Close" > output.csv

# Fetch all repositories for the user
repos=$(curl -s -H "Authorization: token $TOKEN" https://api.github.com/users/$USERNAME/repos?per_page=1000 | jq -r '.[].name')

# Initialize counters
requests_per_minute=0
requests_per_hour=0

# For each repository
for repo in $repos
do
    echo "Processing repository: $repo"
    # Fetch all issues and pull requests
    issues=$(curl -s -H "Authorization: token $TOKEN" https://api.github.com/repos/$USERNAME/$repo/issues?state=all&per_page=1000)
    # Increment counters
    ((requests_per_minute++))
    ((requests_per_hour++))

    # Check if rate limit for minute is reached
    if ((requests_per_minute >= 900)); then
        # Sleep for the remaining seconds in the current minute
        sleep $((60 - $(date +%S)))
        # Reset the per minute counter
        requests_per_minute=0
    fi

    # Check if rate limit for hour is reached
    if ((requests_per_hour >= 5000)); then
        # Sleep for the remaining minutes in the current hour
        sleep $((60 - $(date +%M)) * 60)
        # Reset the per hour counter
        requests_per_hour=0
    fi

    # Sleep for a bit to control the rate
    sleep $((60/900))

    # For each issue or pull request
    issue_count=$(echo "${issues}" | jq -r '.[] | @base64' | wc -l)
    echo "Found $issue_count issues/pull requests in repository: $repo"
    for row in $(echo "${issues}" | jq -r '.[] | @base64'); do
        # Parse the JSON
        _jq() {
            echo ${row} | base64 --decode | jq -r ${1}
        }

        # Extract the required details
        echo "Processing issue/pull request: $(_jq '.title')"

        # Extract the required details
        repo_name=$repo
        title=$(_jq '.title')
        # Add quotation marks around the title
        title="\"$title\""
        comments=$(_jq '.comments')
        url=$(_jq '.html_url')
        date_created=$(_jq '.created_at')
        date_closed=$(_jq '.closed_at')
        state=$(_jq '.state')
        type="Issue"
        if [ "$(_jq '.pull_request')" != "null" ]; then
            type="Pull Request"
        fi
        days_to_close="N/A"
        if [ "$state" = "closed" ]; then
            if [[ "$OSTYPE" == "darwin"* ]]; then
                # macOS
                date1=$(date -j -f "%Y-%m-%dT%H:%M:%SZ" "$date_created" +%s)
                date2=$(date -j -f "%Y-%m-%dT%H:%M:%SZ" "$date_closed" +%s)
            else
                # Linux
                date1=$(date -d"$date_created" +%s)
                date2=$(date -d"$date_closed" +%s)
            fi
            days_to_close=$(( (date2 - date1) / 86400 ))
        fi

        # Write the details to the CSV file
        echo "$repo_name,$title,$comments,$url,$date_created,$date_closed,$state,$type,$days_to_close" >> output.csv
    done
done
