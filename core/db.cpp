// Copyright (C) 2013 Dave Griffiths
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

#include "db.h"
#include <stdio.h>

db::db(const char *fn) :
    m_running(1)
{
    int rc = sqlite3_open(fn, &m_db);
    if(rc)
    {
        m_running=0;
    }
}

db::~db()
{
}

sqlite3_stmt *db::prepare(const char *sql)
{
    sqlite3_stmt *stmt;
    const char *test;
    int rc = sqlite3_prepare(m_db, sql, strlen(sql), &stmt, NULL);

    if( rc != SQLITE_OK )
    {
        return NULL;
    }
    return stmt;
}

void db::bind_text(const char *v, int n, sqlite3_stmt *stmt)
{
    sqlite3_bind_text(stmt, n, v, strlen(v), 0);
}

void db::bind_int(int v, int n, sqlite3_stmt *stmt)
{
    sqlite3_bind_int(stmt, n, v);
}

void db::bind_float(float v, int n, sqlite3_stmt *stmt)
{
    sqlite3_bind_double(stmt, n, (double)v);
}

list *db::run(sqlite3_stmt *stmt)
{
    list *data= new list;
    bool first=true;
    int result=0;
    do {
        result = sqlite3_step(stmt);
        if (result == SQLITE_ROW) { /* can read data */
            if (first)
            {
                // first add the column names
                list *row = new list;
                for (int i=0; i<sqlite3_column_count(stmt); i++) {
                    row->add_to_end(new db::value_node(sqlite3_column_name(stmt, i)));
                }
                data->add_to_end(new db::row_node(row));
                first=false;
            }

            list *row = new list;
            for (int i=0; i<sqlite3_column_count(stmt); i++)
            {
                db::value_node *v=NULL;
                switch (sqlite3_column_type(stmt, i)) {
                case SQLITE_TEXT: v = new db::value_node((char*)sqlite3_column_text(stmt,i)); break;
                case SQLITE_INTEGER: v = new db::value_node(sqlite3_column_int(stmt,i)); break;
                case SQLITE_FLOAT: v = new db::value_node((float)sqlite3_column_double(stmt,i)); break;
                default: v = new db::value_node("NULL"); break; // probably incorrect
                };

                row->add_to_end(v);
            }
            data->add_to_end(new db::row_node(row));
        }
    } while (result == SQLITE_ROW);
    sqlite3_finalize(stmt);
    return data;
}

///////////////////////////////////

/*
list *db::exec(const char *sql)
{
    if (!m_running) return NULL;

    char *err = 0;
    list *data = new list;
    int rc = sqlite3_exec(m_db, sql, callback, data, &err);

    if( rc != SQLITE_OK )
    {
        snprintf(m_status,4096,"SQL error: %s",err);
        //m_running=0;
        sqlite3_free(err);
    }
    else
    {
        snprintf(m_status,4096,"SQL GOOD.");
    }

    return data;
}

unsigned int db::insert(const char *sql)
{
    if (!m_running) return 0;

    char *err = 0;
    list *data = new list;
    int rc = sqlite3_exec(m_db, sql, callback, data, &err);

    if( rc != SQLITE_OK )
    {
        snprintf(m_status,4096,"SQL error: %s",err);
        //m_running=0;
        sqlite3_free(err);
        return 0;
    }
    else
    {
        snprintf(m_status,4096,"SQL GOOD.");
    }

    return sqlite3_last_insert_rowid(m_db);
}
*/
