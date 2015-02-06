#include "core/db.h"
#include "core/list.h"
#include "string.h"
#include <stdio.h>

class db_container
{
public:
    db_container()
    {
        snprintf(m_status,4096,"Startup");
        sqlite3_config(SQLITE_CONFIG_LOG, error, NULL);
    }

    static void error(void *pArg, int iErrCode, const char *zMsg){
        fprintf(stderr,"%s\n",zMsg);
        snprintf(m_status,4096,"(%d) : %s", iErrCode, zMsg);
    }

    static const char *status() { return m_status; }

    void add(const char *fn, db *d)
    {
        m_dbs.add_to_end(new db_node(fn,d));
    }

    db *get(const char *fn)
    {
        db_node *cur=(db_node*)m_dbs.m_head;
        while (cur!=NULL)
        {
            if (!strcmp(fn,cur->m_fn)) return cur->m_db;
            cur=(db_node*)cur->m_next;
        }
        return NULL;
    }

    class db_node : public list::node
    {
    public:
        db_node(const char *fn, db *d)
        {
            m_fn=strdup(fn);
            m_db=d;
        }
        ~db_node()
        {
            free(m_fn);
            delete m_db;
        }

        char *m_fn;
        db *m_db;
    };

private:
    static char m_status[4096];
    list m_dbs;
};
