// ===================================
// Configuration
// ===================================
const API_URL = 'http://localhost:8000';

// ===================================
// Statistics Functions
// ===================================
async function loadStats() {
    try {
        const response = await fetch(`${API_URL}/api/stats`);
        const data = await response.json();
        
        document.getElementById('stat-total').textContent = data.total;
        document.getElementById('stat-completed').textContent = data.completed;
        document.getElementById('stat-pending').textContent = data.pending;
        document.getElementById('stat-rate').textContent = data.completion_rate.toFixed(1) + '%';
    } catch (error) {
        console.error('Error loading stats:', error);
    }
}

// ===================================
// TODO List Functions
// ===================================
async function loadTodos() {
    const status = document.getElementById('filterStatus').value;
    const sortBy = document.getElementById('sortBy').value;
    
    let url = `${API_URL}/todos`;
    const params = [];
    
    if (status) params.push(`completed=${status}`);
    if (sortBy) params.push(`sort_by=${sortBy}&order=desc`);
    
    if (params.length > 0) {
        url += '?' + params.join('&');
    }

    try {
        const response = await fetch(url);
        const data = await response.json();
        displayTodos(data.todos || []);
    } catch (error) {
        console.error('Error loading todos:', error);
        document.getElementById('todoList').innerHTML = '<div class="empty">❌ Error loading tasks</div>';
    }
}

function displayTodos(todos) {
    const todoList = document.getElementById('todoList');
    
    if (todos.length === 0) {
        todoList.innerHTML = '<div class="empty">📭 No tasks to display</div>';
        return;
    }

    todoList.innerHTML = todos.map(todo => createTodoHTML(todo)).join('');
}

function createTodoHTML(todo) {
    const priorityClass = `badge-${todo.priority}`;
    const tags = todo.tags.map(tag => `<span class="tag">#${tag}</span>`).join('');
    
    return `
        <div class="todo-item ${todo.completed ? 'completed' : ''}">
            <div class="todo-header">
                <div class="todo-title">${escapeHtml(todo.title)}</div>
                <span class="badge ${priorityClass}">${todo.priority}</span>
            </div>
            
            ${todo.description ? `<div class="todo-description">${escapeHtml(todo.description)}</div>` : ''}
            
            <div class="todo-meta">
                ${tags}
                ${todo.due_date ? `<span class="todo-date">📅 Due: ${todo.due_date}</span>` : ''}
            </div>
            
            <div class="todo-date">
                ⏰ Created: ${todo.created_at}
            </div>
            
            <div class="todo-actions">
                ${!todo.completed ? `
                    <button class="btn btn-small btn-success" onclick="completeTodo(${todo.id})">
                        ✓ Complete
                    </button>
                ` : ''}
                <button class="btn btn-small btn-danger" onclick="deleteTodo(${todo.id})">
                    🗑️ Delete
                </button>
            </div>
        </div>
    `;
}

// ===================================
// Search Function
// ===================================
function searchTodos() {
    const search = document.getElementById('searchBox').value;
    
    if (search.length >= 2) {
        fetch(`${API_URL}/todos?search=${encodeURIComponent(search)}`)
            .then(res => res.json())
            .then(data => displayTodos(data.todos || []))
            .catch(err => console.error('Search error:', err));
    } else if (search.length === 0) {
        loadTodos();
    }
}

// ===================================
// Create TODO
// ===================================
document.addEventListener('DOMContentLoaded', function() {
    const form = document.getElementById('todoForm');
    
    form.addEventListener('submit', async (e) => {
        e.preventDefault();
        
        const title = document.getElementById('title').value.trim();
        const description = document.getElementById('description').value.trim();
        const priority = document.getElementById('priority').value;
        const due_date_input = document.getElementById('due_date').value;
        const tags_input = document.getElementById('tags').value;
        
        // Validate title
        if (!title) {
            showNotification('⚠️ Title is required', 'error');
            return;
        }
        
        // Convert date format from YYYY-MM-DD to DD/MM/YYYY
        let due_date = null;
        if (due_date_input) {
            const [year, month, day] = due_date_input.split('-');
            due_date = `${day}/${month}/${year}`;
        }
        
        // Parse tags
        const tags = tags_input 
            ? tags_input.split(',').map(t => t.trim()).filter(t => t) 
            : [];
        
        const todo = {
            title,
            description,
            priority,
            due_date,
            tags
        };

        try {
            const response = await fetch(`${API_URL}/todos`, {
                method: 'POST',
                headers: {
                    'Content-Type': 'application/json'
                },
                body: JSON.stringify(todo)
            });

            if (response.ok) {
                form.reset();
                loadTodos();
                loadStats();
                showNotification('✅ Task created successfully!', 'success');
            } else {
                const error = await response.json();
                showNotification('❌ Error: ' + (error.error || 'Unknown error'), 'error');
            }
        } catch (error) {
            console.error('Error creating todo:', error);
            showNotification('❌ Server connection error', 'error');
        }
    });
});

// ===================================
// Complete TODO
// ===================================
async function completeTodo(id) {
    try {
        const response = await fetch(`${API_URL}/todos/${id}`, {
            method: 'PUT',
            headers: {
                'Content-Type': 'application/json'
            },
            body: JSON.stringify({ completed: true })
        });

        if (response.ok) {
            loadTodos();
            loadStats();
            showNotification('✅ Task completed!', 'success');
        } else {
            showNotification('❌ Error completing task', 'error');
        }
    } catch (error) {
        console.error('Error completing todo:', error);
        showNotification('❌ Connection error', 'error');
    }
}

// ===================================
// Delete TODO
// ===================================
async function deleteTodo(id) {
    showConfirm(
        'Are you sure you want to delete this task? This action cannot be undone.',
        async () => {
            try {
                const response = await fetch(`${API_URL}/todos/${id}`, {
                    method: 'DELETE'
                });

                if (response.ok) {
                    loadTodos();
                    loadStats();
                    showNotification('Task deleted successfully', 'success');
                } else {
                    showNotification('Error deleting task', 'error');
                }
            } catch (error) {
                console.error('Error deleting todo:', error);
                showNotification('Connection error', 'error');
            }
        }
    );
}

// ===================================
// Utility Functions
// ===================================
function escapeHtml(text) {
    const map = {
        '&': '&amp;',
        '<': '&lt;',
        '>': '&gt;',
        '"': '&quot;',
        "'": '&#039;'
    };
    return text.replace(/[&<>"']/g, m => map[m]);
}

function showNotification(message, type) {
    const notification = document.createElement('div');
    notification.className = `notification ${type}`;
    
    const icon = type === 'success' ? '✅' : '❌';
    
    notification.innerHTML = `
        <div class="notification-icon">${icon}</div>
        <div class="notification-message">${message}</div>
        <button class="notification-close" onclick="this.parentElement.remove()">×</button>
    `;
    
    document.body.appendChild(notification);
    
    // Auto-remove after 3 seconds
    setTimeout(() => {
        if (notification.parentElement) {
            notification.style.animation = 'slideInRight 0.3s ease-out reverse';
            setTimeout(() => notification.remove(), 300);
        }
    }, 3000);
}

function showConfirm(message, onConfirm, onCancel) {
    const overlay = document.createElement('div');
    overlay.className = 'modal-overlay';
    
    overlay.innerHTML = `
        <div class="modal">
            <div class="modal-header">Confirm Action</div>
            <div class="modal-body">${message}</div>
            <div class="modal-actions">
                <button class="btn btn-small" id="modal-cancel">Cancel</button>
                <button class="btn btn-small btn-danger" id="modal-confirm">Delete</button>
            </div>
        </div>
    `;
    
    document.body.appendChild(overlay);
    
    document.getElementById('modal-cancel').onclick = () => {
        overlay.remove();
        if (onCancel) onCancel();
    };
    
    document.getElementById('modal-confirm').onclick = () => {
        overlay.remove();
        if (onConfirm) onConfirm();
    };
    
    // Close on overlay click
    overlay.onclick = (e) => {
        if (e.target === overlay) {
            overlay.remove();
            if (onCancel) onCancel();
        }
    };
}

// ===================================
// Initialize Application
// ===================================
function init() {
    console.log('🚀 TODO App initialized');
    console.log('📡 API URL:', API_URL);
    
    loadStats();
    loadTodos();
    
    // Auto-refresh every 30 seconds
    setInterval(() => {
        loadStats();
        // Only refresh if not searching
        if (!document.getElementById('searchBox').value) {
            loadTodos();
        }
    }, 30000);
}

// Start the app when DOM is ready
if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', init);
} else {
    init();
}

