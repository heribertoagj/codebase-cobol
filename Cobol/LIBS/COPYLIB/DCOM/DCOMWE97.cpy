      *================================================================*
      *                    B R Q                                       *
      *----------------------------------------------------------------*
      *    BOOK........:   DCOMWE97                                    *
      *    ANALISTA....:   LUIS EDUARDO              - BRQ             *
      *    TAMANHO.....:   352 BYTES                                   *
      *    DATA........:   05/2021                                     *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   AREA DE COMUNICACAO PADRAO DE RETORNO E ERRO*
      *                    ENTRE OS PROGRAMAS CICS(DCOM) X IMS(RISC)   *
      *----------------------------------------------------------------*
      *  DATA       AUTOR             DESCRICAO / MANUTENCAO           *
      *----------------------------------------------------------------*
      *  XX/XX/XXXX XXXXXXXXXXXXXXXXX XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX   *
      *----------------------------------------------------------------*
           05 DCOMWE97-HEADER.
              10 DCOMWE97-COD-LAYOUT  PIC X(08)    VALUE 'DCOMWE97'.
              10 DCOMWE97-TAM-LAYOUT  PIC 9(05)    VALUE 00356.
      *
           05  DCOMWE97-BLOCO-RETORNO.
               07  DCOMWE97-R-RETORNO           PIC  9(02).
               07  DCOMWE97-R-ERRO              PIC  X(04).
               07  DCOMWE97-R-MENSAGEM          PIC  X(80).
      *
           05  DCOMWE97-ERRO-TIPO.
               07  DCOMWE97-TIPO-ERRO           PIC  X(02).
                   88  TIPO-E97-CORRETO-ONLINE  VALUE  'MQ' 'DB' 'CI'
                                                       'MO' 'AR' 'LI'.
                   88  TIPO-E97-CORRETO-BATCH   VALUE  'MQ' 'DB'
                                                       'MO' 'AR' 'LI'.
                   88  ERRO-E97-MQ              VALUE  'MQ'.
                   88  ERRO-E97-DB2             VALUE  'DB'.
                   88  ERRO-E97-CICS            VALUE  'CI'.
                   88  ERRO-E97-MODULO          VALUE  'MO'.
                   88  ERRO-E97-ARQUIVO         VALUE  'AR'.
                   88  ERRO-E97-LIVRE           VALUE  'LI'.
               07  DCOMWE97-NOME-PROGRAMA       PIC  X(08).
               07  DCOMWE97-IDEN-PARAGRAFO      PIC  X(30).
      *
           05  DCOMWE97-ERRO-MODULO.
               07  DCOMWE97-NOME-MODULO         PIC  X(08).
      *
           05 DCOMWE97-ERR-DB2.
               07 DCOMWE97-NOME-TABELA          PIC  X(32).
               07 DCOMWE97-COMANDO-DB2          PIC  X(09).
                   88 DB2-E97-SELECT            VALUE 'SELECT   '.
                   88 DB2-E97-UPDATE            VALUE 'UPDATE   '.
                   88 DB2-E97-INSERT            VALUE 'INSERT   '.
                   88 DB2-E97-DELETE            VALUE 'DELETE   '.
                   88 DB2-E97-OPEN              VALUE 'OPEN     '.
                   88 DB2-E97-FETCH             VALUE 'FETCH    '.
                   88 DB2-E97-CLOSE             VALUE 'CLOSE    '.
                   88 DB2-E97-ASSOCIATE         VALUE 'ASSOCIATE'.
                   88 DB2-E97-ALLOCATE          VALUE 'ALLOCATE '.
                   88 DB2-E97-CALL-SP           VALUE 'CALL-SP  '.
               07  DCOMWE97-STORED-PROC         PIC  X(08).
               07  DCOMWE97-LOCAL               PIC  X(16).
               07  DCOMWE97-SQLCODE2            PIC S9(03).
               07  DCOMWE97-SQLSTATE            PIC  X(05).
               07  DCOMWE97-SQLCA.
                   09  DCOMWE97-SQLCAID         PIC  X(08).
                   09  DCOMWE97-SQLCABC         PIC S9(09) COMP-4.
                   09  DCOMWE97-SQLCODE         PIC S9(09) COMP-4.
                   09  DCOMWE97-SQLERRM.
                       49  DCOMWE97-SQLERRML    PIC S9(04) COMP-4.
                       49  DCOMWE97-SQLERRMC    PIC  X(70).
                   09  DCOMWE97-SQLERRP         PIC  X(08).
                   09  DCOMWE97-SQLERRD         OCCURS 6 TIMES
                                                PIC S9(09) COMP-4.
                   09  DCOMWE97-SQLWARN.
                       10  DCOMWE97-SQLWARN0    PIC  X.
                       10  DCOMWE97-SQLWARN1    PIC  X.
                       10  DCOMWE97-SQLWARN2    PIC  X.
                       10  DCOMWE97-SQLWARN3    PIC  X.
                       10  DCOMWE97-SQLWARN4    PIC  X.
                       10  DCOMWE97-SQLWARN5    PIC  X.
                       10  DCOMWE97-SQLWARN6    PIC  X.
                       10  DCOMWE97-SQLWARN7    PIC  X.
                   09  DCOMWE97-SQLEXT.
                       10  DCOMWE97-SQLWARN8    PIC  X.
                       10  DCOMWE97-SQLWARN9    PIC  X.
                       10  DCOMWE97-SQLWARNA    PIC  X.
                       10  DCOMWE97-SQLSTATE2   PIC  X(05).
      *----------------------------------------------------------------*
