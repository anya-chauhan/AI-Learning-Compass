
#Work in progress: this api will replace the direct gpt api call in R
from flask import Flask, request, jsonify
import sqlite3
import requests
import time
from typing import Optional
import os
from dotenv import load_dotenv

app = Flask(__name__)

# Load environment variables
load_dotenv()
DATABASE = 'responses.db'
API_KEY = os.getenv('OPENAI_API_KEY')

class DatabaseError(Exception):
    """Custom exception for database operations"""
    pass

def get_db_connection():
    """Create and return a database connection"""
    try:
        conn = sqlite3.connect(DATABASE)
        conn.row_factory = sqlite3.Row
        return conn
    except sqlite3.Error as e:
        raise DatabaseError(f"Database connection error: {e}")

def init_db():
    """Initialize the database with proper error handling"""
    try:
        with get_db_connection() as conn:
            cursor = conn.cursor()
            cursor.execute('''
                CREATE TABLE IF NOT EXISTS gpt_responses (
                    id INTEGER PRIMARY KEY AUTOINCREMENT,
                    node TEXT NOT NULL,
                    prompt TEXT NOT NULL,
                    response TEXT NOT NULL,
                    timestamp TEXT NOT NULL,
                    UNIQUE(node)
                )
            ''')
            conn.commit()
    except DatabaseError as e:
        app.logger.error(f"Database initialization failed: {e}")
        raise

def get_response_from_db(node: str) -> Optional[str]:
    """Query database for a stored response with error handling"""
    try:
        with get_db_connection() as conn:
            cursor = conn.cursor()
            cursor.execute('SELECT response FROM gpt_responses WHERE node = ?', (node,))
            result = cursor.fetchone()
            return result['response'] if result else None
    except DatabaseError as e:
        app.logger.error(f"Database query failed: {e}")
        return None

def store_response_in_db(node: str, prompt: str, response: str) -> bool:
    """Store response in the database with error handling"""
    try:
        with get_db_connection() as conn:
            cursor = conn.cursor()
            cursor.execute(
                'INSERT OR REPLACE INTO gpt_responses (node, prompt, response, timestamp) VALUES (?, ?, ?, ?)',
                (node, prompt, response, time.strftime('%Y-%m-%d %H:%M:%S'))
            )
            conn.commit()
            return True
    except DatabaseError as e:
        app.logger.error(f"Failed to store response: {e}")
        return False

def call_gpt_api(prompt: str) -> Optional[str]:
    """Call GPT API with error handling and timeout"""
    if not API_KEY:
        app.logger.error("API key not found in environment variables")
        return None

    url = "https://api.openai.com/v1/chat/completions"
    headers = {
        "Authorization": f"Bearer {API_KEY}",
        "Content-Type": "application/json"
    }
    data = {
        "model": "gpt-3.5-turbo",
        "messages": [{"role": "user", "content": prompt}],
        "max_tokens": 300
    }

    try:
        response = requests.post(url, headers=headers, json=data, timeout=10)
        response.raise_for_status()
        return response.json()["choices"][0]["message"]["content"]
    except requests.exceptions.RequestException as e:
        app.logger.error(f"API call failed: {e}")
        return None

@app.route('/get_response', methods=['POST'])
def get_response():
    """Main endpoint for retrieving or generating responses"""
    try:
        data = request.get_json()
        if not data or 'node' not in data or 'prompt' not in data:
            return jsonify({"error": "Missing required fields"}), 400

        node = data['node']
        prompt = data['prompt']

        # Check for stored response
        stored_response = get_response_from_db(node)
        if stored_response:
            return jsonify({
                "response": stored_response,
                "source": "database"
            })

        # Generate new response
        gpt_response = call_gpt_api(prompt)
        if gpt_response:
            store_success = store_response_in_db(node, prompt, gpt_response)
            return jsonify({
                "response": gpt_response,
                "source": "gpt_api",
                "cached": store_success
            })
        
        return jsonify({"error": "Failed to generate response"}), 500

    except Exception as e:
        app.logger.error(f"Unexpected error: {e}")
        return jsonify({"error": "Internal server error"}), 500

if __name__ == '__main__':
    init_db()
    app.run(debug=True)
