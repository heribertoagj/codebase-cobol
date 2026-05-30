      ******************************************************************
      *                       INICIO DO BOOK                           *
      ******************************************************************
      * NOME BOOK : BVVEWERR                                           *
      * DESCRICAO : BOOK PARA TRATAMENTO DE ERROS                      *
      * DATA      : 04/02/2014                                         *
      * AUTOR     : FABRICIO MASAKIYO MANCINI                          *
      * ANALISTA  : MAXIMILIANO BARBOSA                                *
      * EMPRESA   : 7COMM                                              *
      * SISTEMA   : BVVE                                               *
      * TAMANHO   : 00308 BYTES                                        *
      ******************************************************************
      *                                                                *
      ******************************************************************
      *                    ERRO DO TIPO APLICACAO                      *
      ******************************************************************
           05 BVVEWERR-INFO-ERRO.
               10 BVVEWERR-ERR-APL.
                   15 FILLER                   PIC  X(010) VALUE
                               'ERRO APL. '.
                   15 BVVEWERR-APL-MODULO      PIC  X(008) VALUE SPACES.
                   15 FILLER                   PIC  X(001) VALUE
                               '-'.
                   15 BVVEWERR-APL-MSGEM       PIC  X(050) VALUE SPACES.
                   15 FILLER                   PIC  X(005) VALUE
                               '-LOC:'.
                   15 BVVEWERR-APL-LOCAL       PIC  X(004) VALUE SPACES.
      ******************************************************************
      *                    ERRO DO TIPO DB2                            *
      ******************************************************************
               10 BVVEWERR-ERR-DB2.
                   15 FILLER                   PIC  X(010) VALUE
                               'ERRO DB2. '.
                   15 BVVEWERR-DB2-MODULO      PIC  X(008).
                   15 FILLER                   PIC  X(001) VALUE
                               ' '.
                   15 BVVEWERR-DB2-COMANDO     PIC  X(007).
                       88 ERR-DB2-SELECT       VALUE 'SELECT '.
                       88 ERR-DB2-UPDATE       VALUE 'UPDATE '.
                       88 ERR-DB2-INSERT       VALUE 'INSERT '.
                       88 ERR-DB2-DELETE       VALUE 'DELETE '.
                       88 ERR-DB2-OPEN         VALUE 'OPEN   '.
                       88 ERR-DB2-FETCH        VALUE 'FETCH  '.
                       88 ERR-DB2-CLOSE        VALUE 'CLOSE  '.
                   15 BVVEWERR-DB2-TABELA      PIC  X(018).
                   15 FILLER                   PIC  X(009) VALUE
                               ' SQLCODE:'.
                   15 BVVEWERR-DB2-SQLCODE     PIC +9(009).
                   15 FILLER                   PIC  X(005) VALUE
                               ' LOC:'.
                   15 BVVEWERR-DB2-LOCAL       PIC  X(004).
      ******************************************************************
      *                    ERRO DO TIPO MQ                             *
      ******************************************************************
               10 BVVEWERR-ERR-MQ.
                   15 FILLER                   PIC  X(009) VALUE
                               'ERRO MQ. '.
                   15 BVVEWERR-MQ-FILA         PIC  X(048).
                   15 FILLER                   PIC  X(003) VALUE
                               '-F:'.
                   15 BVVEWERR-MQ-FUNCAO       PIC  X(002).
                   15 FILLER                   PIC  X(001) VALUE
                               '-'.
                   15 BVVEWERR-MQ-RET-CODE     PIC  9(004) .
                   15 FILLER                   PIC  X(001) VALUE
                               '-'.
                   15 BVVEWERR-MQ-RSON-CODE    PIC  9(004) .
                   15 FILLER                   PIC  X(003) VALUE
                               ' L:'.
                   15 BVVEWERR-MQ-LOCAL        PIC  X(004).
      ******************************************************************
      *                        ERRO CICS                               *
      ******************************************************************
               10 BVVEWERR-ERR-CICS.
                   15 FILLER                   PIC  X(011) VALUE
                               'ERRO CICS. '.
                   15 BVVEWERR-CICS-MODULO     PIC  X(008).
                   15 BVVEWERR-CICS-TEX        PIC  X(012) VALUE
                               ' AO ACESSAR '.
                   15 BVVEWERR-CICS-ACESSADO   PIC  X(008).
                   15 FILLER                   PIC  X(006) VALUE
                               ' RESP:'.
                   15 BVVEWERR-CICS-EIBRESP    PIC +9(008).
                   15 FILLER                   PIC  X(007) VALUE
                               ' RESP2:'.
                   15 BVVEWERR-CICS-EIBRESP2   PIC +9(008).
                   15 FILLER                   PIC  X(005) VALUE
                               ' LOC:'.
                   15 BVVEWERR-CICS-LOCAL      PIC  X(004).
      *                                                                *
      ******************************************************************
      *                          FIM DO BOOK                           *
      *****************************************************************
