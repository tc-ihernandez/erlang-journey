#!/bin/bash

# Demo Data Seeding Script for TODO API
# This script creates sample tasks to demonstrate all features

API_URL="http://localhost:8000"

echo "🌱 Seeding demo data for TODO API..."
echo "======================================"
echo ""

# Function to create a TODO
create_todo() {
    curl -s -X POST "$API_URL/todos" \
        -H "Content-Type: application/json" \
        -d "$1" > /dev/null
    echo "✓ Created: $(echo $1 | grep -o '"title":"[^"]*"' | cut -d'"' -f4)"
}

# 1. Urgent task with due date
create_todo '{
    "title": "Deploy Production Release v2.0",
    "description": "Deploy the new version to production servers before deadline",
    "priority": "urgent",
    "tags": ["devops", "production", "critical"],
    "due_date": "25/12/2025"
}'

# 2. High priority with tags
create_todo '{
    "title": "Fix Critical Security Vulnerability",
    "description": "Patch CVE-2024-1234 in authentication module",
    "priority": "high",
    "tags": ["security", "backend", "urgent"]
}'

# 3. Learning task
create_todo '{
    "title": "Complete Erlang OTP Course",
    "description": "Finish chapters 8-12 on supervision trees and gen_server patterns",
    "priority": "high",
    "tags": ["learning", "erlang", "otp"],
    "due_date": "31/12/2025"
}'

# 4. Meeting
create_todo '{
    "title": "Team Standup Meeting",
    "description": "Daily sync with the development team",
    "priority": "medium",
    "tags": ["meeting", "team"],
    "due_date": "11/12/2025 09:00:00"
}'

# 5. Documentation
create_todo '{
    "title": "Write API Documentation",
    "description": "Document all new endpoints and features for the REST API",
    "priority": "medium",
    "tags": ["documentation", "api", "technical-writing"]
}'

# 6. Code review
create_todo '{
    "title": "Review Pull Request #247",
    "description": "Code review for new authentication middleware implementation",
    "priority": "high",
    "tags": ["code-review", "backend", "security"],
    "due_date": "12/12/2025"
}'

# 7. Testing
create_todo '{
    "title": "Write Unit Tests for Payment Module",
    "description": "Achieve 90% code coverage for the payment processing system",
    "priority": "medium",
    "tags": ["testing", "quality-assurance", "backend"]
}'

# 8. Low priority task
create_todo '{
    "title": "Update Dependencies",
    "description": "Check and update all npm packages to latest stable versions",
    "priority": "low",
    "tags": ["maintenance", "dependencies"]
}'

# 9. Design task
create_todo '{
    "title": "Design New Landing Page",
    "description": "Create mockups for the new marketing landing page",
    "priority": "medium",
    "tags": ["design", "frontend", "ui-ux"],
    "due_date": "20/12/2025"
}'

# 10. Database optimization
create_todo '{
    "title": "Optimize Database Queries",
    "description": "Improve performance of slow queries in user dashboard",
    "priority": "high",
    "tags": ["database", "performance", "backend"]
}'

# 11. Bug fix
create_todo '{
    "title": "Fix Mobile Responsive Issues",
    "description": "Address layout problems on iOS Safari and Android Chrome",
    "priority": "high",
    "tags": ["bug", "frontend", "mobile"],
    "due_date": "15/12/2025"
}'

# 12. Content task
create_todo '{
    "title": "Prepare Conference Presentation",
    "description": "Create slides for ErlangFactory 2025 talk on distributed systems",
    "priority": "medium",
    "tags": ["conference", "presentation", "erlang"]
}'

echo ""
echo "======================================"
echo "✅ Demo data seeded successfully!"
echo ""
echo "📊 Created 12 sample tasks with:"
echo "   • 3 Urgent/High priority tasks"
echo "   • Various due dates"
echo "   • Multiple tags per task"
echo "   • Different categories"
echo ""
echo "🌐 Open the frontend: public/index.html"
echo "📡 API running at: $API_URL"
echo ""

