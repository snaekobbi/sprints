#!/usr/bin/env zsh
for issue in $1; do
    ghi show $issue > issue_$issue.bak
    curl https://api.github.com/repos/snaekobbi/sprints/issues/$issue 2>/dev/null > tmp.json
    eval title=$( cat tmp.json | jq '.title' )
    eval body=$( cat tmp.json | jq '.body' | sed 's/\`/\\\`/g' )
    regex='^## .*sprint#\([1-9][0-9]*\)$'
    if ! echo "$body" | dos2unix | grep "$regex" >/dev/null; then
        echo "Issue $issue has no ## sprint#xxx heading!"
        exit 1
    fi
    new_issue=$(
        ghi open -m "$(
            echo "$body" | dos2unix | sed -n "/$regex/q;p" \
            | cat <(echo "$title") - <(echo "## [:rewind:](https://github.com/snaekobbi/sprints/issues/$issue) sprint#?")
        )" | head -n 1 | sed 's/^#\([1-9][0-9]*\):.*$/\1/'
    )
    ghi show $new_issue
    ghi edit -m "$(
        echo "$title\n$body" | dos2unix | sed "/$regex/ s|$| [:fast_forward:](https://github.com/snaekobbi/sprints/issues/$new_issue)|g"
    )" $issue
done
