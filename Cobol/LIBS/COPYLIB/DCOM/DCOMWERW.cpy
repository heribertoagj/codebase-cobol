      *================================================================*
      * NOME BOOK..:  DCOMWERW - BOOK AUXILIAR PARA FORMATACAO DE MSG  *
      *                          DE ERRO - FUNCIONAIS E COORDENADORES  *
      * DATA.......:  ABRIL/2023                                       *
      * AUTOR......:  LUCIANDRA R SILVEIRA                             *
      * COMPONENTE.:  DCOM - DESCONTO COMERCIAL                        *
      *================================================================*
      *
      *-> ERRO DB2
      *------------
       05 DCOMWERW-MSGS-ERRO.
         10 DCOMWERW-DB2.
            15 FILLER                  PIC  X(011)         VALUE
                '(EIBTASKN: '.
            15 DCOMWERW-DB2-EIBTASKN   PIC -------9        VALUE ZEROS.
            15 FILLER                  PIC  X(002)         VALUE
                ') '.
            15 FILLER                  PIC  X(009)         VALUE
                'PROGRAMA '.
            15 DCOMWERW-DB2-PROGRAMA   PIC  X(008)         VALUE SPACES.
            15 FILLER                  PIC  X(019)         VALUE
               ': ERRO DB2 TABELA: '.
            15 DCOMWERW-DB2-TABELA     PIC  X(032)         VALUE SPACES.
            15 FILLER                  PIC  X(012)         VALUE
               ' / COMANDO: '.
            15 DCOMWERW-DB2-COMANDO    PIC  X(009).
               88 DCOMWERW-DB2-SELECT  VALUE 'SELECT   '.
               88 DCOMWERW-DB2-UPDATE  VALUE 'UPDATE   '.
               88 DCOMWERW-DB2-INSERT  VALUE 'INSERT   '.
               88 DCOMWERW-DB2-DELETE  VALUE 'DELETE   '.
               88 DCOMWERW-DB2-OPEN    VALUE 'OPEN     '.
               88 DCOMWERW-DB2-FETCH   VALUE 'FETCH    '.
               88 DCOMWERW-DB2-CLOSE   VALUE 'CLOSE    '.
               88 DCOMWERW-DB2-ASSOCIATE
                                       VALUE 'ASSOCIATE'.
               88 DCOMWERW-DB2-ALLOCATE
                                       VALUE 'ALLOCATE '.
               88 DCOMWERW-DB2-CALL-SP VALUE 'CALL-SP  '.
            15 FILLER                  PIC  X(010)         VALUE
               ' / LOCAL: '.
            15 DCOMWERW-DB2-LOCAL      PIC  X(004)         VALUE SPACES.
            15 FILLER                  PIC  X(012)         VALUE
               ' / SQLCODE: '.
            15 DCOMWERW-DB2-SQLCODE    PIC ---------9      VALUE ZEROS.
            15 FILLER                  PIC  X(003)         VALUE ' - '.
            15 DCOMWERW-DB2-MSG-ERRO   PIC  X(100)         VALUE SPACES.
            15 FILLER                  PIC  X(001)         VALUE SPACES.
      *
      *-> ERRO DE MODULO
      *-----------------
         10 DCOMWERW-MOD.
            15 FILLER                  PIC  X(011)         VALUE
                '(EIBTASKN: '.
            15 DCOMWERW-MOD-EIBTASKN   PIC -------9        VALUE ZEROS.
            15 FILLER                  PIC  X(002)         VALUE
                ') '.
            15 FILLER                  PIC  X(009)         VALUE
                'PROGRAMA '.
            15 DCOMWERW-MOD-PROGRAMA   PIC  X(008)         VALUE SPACES.
            15 FILLER                  PIC  X(015)         VALUE
               ': ERRO MODULO: '.
            15 DCOMWERW-MOD-MODULO     PIC  X(008)         VALUE SPACES.
            15 FILLER                  PIC  X(012)         VALUE
               ' / COD-RET: '.
            15 DCOMWERW-MOD-COD-RET    PIC  9(002)         VALUE ZEROS.
            15 FILLER                  PIC  X(010)         VALUE
               ' / LOCAL: '.
            15 DCOMWERW-MOD-LOCAL      PIC  X(004)         VALUE SPACES.
            15 FILLER                  PIC  X(012)         VALUE
               ' / COD-MEN: '.
            15 DCOMWERW-MOD-COD-MSG    PIC  X(008)         VALUE ZEROS.
            15 FILLER                  PIC  X(003)         VALUE
               ' - '.
            15 DCOMWERW-MOD-MSG-ERRO   PIC  X(130)         VALUE SPACES.
            15 FILLER                  PIC  X(008)         VALUE SPACES.
      *
      *-> ERRO DE CICS
      *-----------------
         10 DCOMWERW-CIC.
            15 FILLER                  PIC  X(011)         VALUE
                '(EIBTASKN: '.
            15 DCOMWERW-CIC-EIBTASKN   PIC -------9        VALUE ZEROS.
            15 FILLER                  PIC  X(002)         VALUE
                ') '.
            15 FILLER                  PIC  X(009)         VALUE
                'PROGRAMA '.
            15 DCOMWERW-CIC-PROGRAMA   PIC  X(008)         VALUE SPACES.
            15 FILLER                  PIC  X(020)         VALUE
               ': ERRO CICS MODULO: '.
            15 DCOMWERW-CIC-MODULO     PIC  X(008)         VALUE SPACES.
            15 FILLER                  PIC  X(012)         VALUE
               ' / EIBRESP: '.
            15 DCOMWERW-CIC-EIBRESP    PIC --------9       VALUE ZEROS.
            15 FILLER                  PIC  X(013)         VALUE
               ' / EIBRESP2: '.
            15 DCOMWERW-CIC-EIBRESP2   PIC --------9       VALUE ZEROS.
            15 FILLER                  PIC  X(010)         VALUE
               ' / LOCAL: '.
            15 DCOMWERW-CIC-LOCAL      PIC  X(004)         VALUE SPACES.
            15 FILLER                  PIC  X(003)         VALUE ' - '.
            15 DCOMWERW-CIC-MSG-ERRO   PIC  X(100)         VALUE SPACES.
            15 FILLER                  PIC  X(024)         VALUE SPACES.
      *
      *-> ERRO LIVRE
      *-----------------
         10 DCOMWERW-LIV.
            15 FILLER                  PIC  X(011)         VALUE
                '(EIBTASKN: '.
            15 DCOMWERW-LIV-EIBTASKN   PIC -------9        VALUE ZEROS.
            15 FILLER                  PIC  X(002)         VALUE
                ') '.
            15 FILLER                  PIC  X(009)         VALUE
                'PROGRAMA '.
            15 DCOMWERW-LIV-PROGRAMA   PIC  X(008)         VALUE SPACES.
            15 FILLER                  PIC  X(020)         VALUE
               ': ERRO COD-RETORNO: '.
            15 DCOMWERW-LIV-COD-RET    PIC  9(002)         VALUE ZEROS.
            15 FILLER                  PIC  X(010)         VALUE
               ' / LOCAL: '.
            15 DCOMWERW-LIV-LOCAL      PIC  X(004)         VALUE SPACES.
            15 FILLER                  PIC  X(017)         VALUE
               ' / COD-MENSAGEM: '.
            15 DCOMWERW-LIV-COD-MSG    PIC  X(008)         VALUE ZEROS.
            15 FILLER                  PIC  X(003)         VALUE
               ' - '.
            15 DCOMWERW-LIV-MSG-ERRO   PIC  X(100)         VALUE SPACES.
            15 FILLER                  PIC  X(048)         VALUE SPACES.
      *
      *-> ERRO DE CONSISTENCIA
      *------------------------
         10 DCOMWERW-CON.
            15 FILLER                  PIC  X(011)         VALUE
                '(EIBTASKN: '.
            15 DCOMWERW-CON-EIBTASKN   PIC -------9        VALUE ZEROS.
            15 FILLER                  PIC  X(002)         VALUE
                ') '.
            15 FILLER                  PIC  X(009)         VALUE
                'PROGRAMA '.
            15 DCOMWERW-CON-PROGRAMA   PIC  X(008)         VALUE SPACES.
            15 FILLER                  PIC  X(009)         VALUE
               ': CAMPO: '.
            15 DCOMWERW-CON-CAMPO      PIC  X(030)         VALUE SPACES.
            15 FILLER                  PIC  X(027)         VALUE
               ' INVALIDO OU NAO PREENCHIDO'.
            15 FILLER                  PIC  X(146)         VALUE SPACES.
      *
      *-> ERRO DE CONSISTENCIA DE ACESSO SQL
      *---------------------------------------
         05 DCOMWERW-E01.
            10 FILLER                  PIC  X(010)         VALUE
               'PROGRAMA: '.
            10 DCOMWERW-E01-PROGRAMA   PIC  X(008)         VALUE SPACES.
            10 FILLER                  PIC  X(011)         VALUE
               ' - TABELA: '.
            10 DCOMWERW-E01-TABELA     PIC  X(008)         VALUE SPACES.
            10 FILLER                  PIC  X(014)         VALUE
               ' - INSTRUCAO: '.
            10 DCOMWERW-E01-INSTRUCAO  PIC  X(010)         VALUE SPACES.
            10 FILLER                  PIC  X(013)         VALUE
               ' - EIBTASKN: '.
            10 DCOMWERW-E01-EIBTASKN   PIC  -------9       VALUE ZEROS.
      *
      *-> ERRO DE CONSISTENCIA DE INSTRUCAO DE ACESSO SQL
      *---------------------------------------------------
         05 DCOMWERW-E02.
            10 FILLER                  PIC  X(010)         VALUE
               'PROGRAMA: '.
            10 DCOMWERW-E02-PROGRAMA   PIC  X(008)         VALUE SPACES.
            10 FILLER                  PIC  X(014)         VALUE
               ' - INSTRUCAO: '.
            10 DCOMWERW-E02-INSTRUCAO  PIC  X(010)         VALUE SPACES.
            10 FILLER                  PIC  X(013)         VALUE
               ' - EIBTASKN: '.
            10 DCOMWERW-E02-EIBTASKN   PIC -------9        VALUE ZEROS.
      *
      *================================================================*
