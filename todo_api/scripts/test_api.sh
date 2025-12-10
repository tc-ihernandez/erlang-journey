#!/bin/bash

# Test script for TODO API
# Run this after starting the application with: rebar3 shell

BASE_URL="http://localhost:8000"
GREEN='\033[0;32m'
RED='\033[0;31m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

echo "=========================================="
echo "TODO API - Test Suite"
echo "=========================================="
echo ""

# Function to print test header
test_header() {
    echo -e "${BLUE}$1${NC}"
}

# Function to check if API is running
check_api() {
    if ! curl -s -f $BASE_URL/health > /dev/null 2>&1; then
        echo -e "${RED}✗ API is not running!${NC}"
        echo "Please start the API first with: rebar3 shell"
        exit 1
    fi
}

# Check if API is running
check_api

# Test 1: Health Check
test_header "Test 1: Health Check"
response=$(curl -s $BASE_URL/health)
echo "Response: $response"
if echo "$response" | grep -q "ok"; then
    echo -e "${GREEN}✓ Health check passed${NC}"
else
    echo -e "${RED}✗ Health check failed${NC}"
fi
echo ""

# Test 2: Create TODO #1
test_header "Test 2: Create TODO #1"
response=$(curl -s -X POST $BASE_URL/todos \
  -H "Content-Type: application/json" \
  -d '{"title":"Learn Erlang basics","description":"Study syntax and OTP"}')
echo "Response: $response"
if echo "$response" | grep -q "Learn Erlang basics"; then
    echo -e "${GREEN}✓ TODO created successfully${NC}"
    # Extract ID for later use
    TODO_ID_1=$(echo "$response" | grep -o '"id":[0-9]*' | grep -o '[0-9]*' | head -1)
    echo "Created TODO with ID: $TODO_ID_1"
else
    echo -e "${RED}✗ Failed to create TODO${NC}"
fi
echo ""

# Test 3: Create TODO #2
test_header "Test 3: Create TODO #2"
response=$(curl -s -X POST $BASE_URL/todos \
  -H "Content-Type: application/json" \
  -d '{"title":"Build a CRUD API","description":"Using Elli and Mnesia"}')
echo "Response: $response"
if echo "$response" | grep -q "Build a CRUD API"; then
    echo -e "${GREEN}✓ TODO created successfully${NC}"
    TODO_ID_2=$(echo "$response" | grep -o '"id":[0-9]*' | grep -o '[0-9]*' | head -1)
    echo "Created TODO with ID: $TODO_ID_2"
else
    echo -e "${RED}✗ Failed to create TODO${NC}"
fi
echo ""

# Test 4: Create TODO without title (should fail)
test_header "Test 4: Create TODO without title (should fail)"
response=$(curl -s -X POST $BASE_URL/todos \
  -H "Content-Type: application/json" \
  -d '{"description":"This should fail"}')
echo "Response: $response"
if echo "$response" | grep -q "error"; then
    echo -e "${GREEN}✓ Validation working correctly${NC}"
else
    echo -e "${RED}✗ Validation not working${NC}"
fi
echo ""

# Test 5: List all TODOs
test_header "Test 5: List all TODOs"
response=$(curl -s $BASE_URL/todos)
echo "Response: $response"
if echo "$response" | grep -q "todos"; then
    echo -e "${GREEN}✓ Retrieved TODO list${NC}"
    count=$(echo "$response" | grep -o '"id":' | wc -l)
    echo "Found $count TODOs"
else
    echo -e "${RED}✗ Failed to retrieve TODOs${NC}"
fi
echo ""

# Test 6: Get specific TODO
test_header "Test 6: Get specific TODO"
if [ ! -z "$TODO_ID_1" ]; then
    response=$(curl -s $BASE_URL/todos/$TODO_ID_1)
    echo "Response: $response"
    if echo "$response" | grep -q "Learn Erlang basics"; then
        echo -e "${GREEN}✓ Retrieved specific TODO${NC}"
    else
        echo -e "${RED}✗ Failed to retrieve TODO${NC}"
    fi
else
    echo -e "${RED}✗ No TODO ID available${NC}"
fi
echo ""

# Test 7: Update TODO (mark as completed)
test_header "Test 7: Update TODO (mark as completed)"
if [ ! -z "$TODO_ID_1" ]; then
    response=$(curl -s -X PUT $BASE_URL/todos/$TODO_ID_1 \
      -H "Content-Type: application/json" \
      -d '{"completed":true}')
    echo "Response: $response"
    if echo "$response" | grep -q '"completed":true'; then
        echo -e "${GREEN}✓ TODO updated successfully${NC}"
    else
        echo -e "${RED}✗ Failed to update TODO${NC}"
    fi
else
    echo -e "${RED}✗ No TODO ID available${NC}"
fi
echo ""

# Test 8: Update TODO title
test_header "Test 8: Update TODO title"
if [ ! -z "$TODO_ID_2" ]; then
    response=$(curl -s -X PUT $BASE_URL/todos/$TODO_ID_2 \
      -H "Content-Type: application/json" \
      -d '{"title":"Build an awesome CRUD API"}')
    echo "Response: $response"
    if echo "$response" | grep -q "awesome"; then
        echo -e "${GREEN}✓ TODO title updated${NC}"
    else
        echo -e "${RED}✗ Failed to update title${NC}"
    fi
else
    echo -e "${RED}✗ No TODO ID available${NC}"
fi
echo ""

# Test 9: Get non-existent TODO (should return 404)
test_header "Test 9: Get non-existent TODO (should return 404)"
response=$(curl -s -w "\n%{http_code}" $BASE_URL/todos/999999)
http_code=$(echo "$response" | tail -1)
echo "HTTP Status: $http_code"
if [ "$http_code" = "404" ]; then
    echo -e "${GREEN}✓ 404 error handled correctly${NC}"
else
    echo -e "${RED}✗ Expected 404, got $http_code${NC}"
fi
echo ""

# Test 10: Delete TODO
test_header "Test 10: Delete TODO"
if [ ! -z "$TODO_ID_2" ]; then
    response=$(curl -s -w "\n%{http_code}" -X DELETE $BASE_URL/todos/$TODO_ID_2)
    http_code=$(echo "$response" | tail -1)
    echo "HTTP Status: $http_code"
    if [ "$http_code" = "204" ]; then
        echo -e "${GREEN}✓ TODO deleted successfully${NC}"
    else
        echo -e "${RED}✗ Failed to delete TODO (expected 204, got $http_code)${NC}"
    fi
else
    echo -e "${RED}✗ No TODO ID available${NC}"
fi
echo ""

# Test 11: Verify deletion
test_header "Test 11: Verify deletion"
if [ ! -z "$TODO_ID_2" ]; then
    response=$(curl -s -w "\n%{http_code}" $BASE_URL/todos/$TODO_ID_2)
    http_code=$(echo "$response" | tail -1)
    echo "HTTP Status: $http_code"
    if [ "$http_code" = "404" ]; then
        echo -e "${GREEN}✓ TODO successfully deleted (404 returned)${NC}"
    else
        echo -e "${RED}✗ TODO still exists${NC}"
    fi
else
    echo -e "${RED}✗ No TODO ID available${NC}"
fi
echo ""

# Test 12: Invalid JSON (should return 400)
test_header "Test 12: Invalid JSON (should return 400)"
response=$(curl -s -w "\n%{http_code}" -X POST $BASE_URL/todos \
  -H "Content-Type: application/json" \
  -d 'invalid json')
http_code=$(echo "$response" | tail -1)
echo "HTTP Status: $http_code"
if [ "$http_code" = "400" ]; then
    echo -e "${GREEN}✓ Invalid JSON handled correctly${NC}"
else
    echo -e "${RED}✗ Expected 400, got $http_code${NC}"
fi
echo ""

# Final summary
echo "=========================================="
echo "Test Suite Complete!"
echo "=========================================="
echo ""
echo "Check the results above. All tests with ✓ passed."
echo ""
echo "To see all remaining TODOs:"
echo "  curl http://localhost:8000/todos | jq"
echo ""

