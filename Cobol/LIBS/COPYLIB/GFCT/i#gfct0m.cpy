      *****************************************************************
      * NOME DA INC - I#GFCT0M                                        *
      * DESCRICAO   - MANUTENCAO TELAS GFCT                           *
      *               COMUNICACAO IMS X WEB - SBAT3000 - ERROS        *
      * TAMANHO     - 230                                             *
      * DATA        - 22.10.2004                                      *
      * RESPONSAVEL - MAURICIO - PROCWORK                             *
      *****************************************************************
       01  GFCT0M-AREA-ERROS.
           03  GFCT0M-TRANSACAO                     PIC  X(008).
           03  GFCT0M-ERRO-AREAS.
               05  GFCT0M-AREA-COMUM.
                   10 GFCT0M-TIPO-ACESSO            PIC  X(003).
                   10 GFCT0M-PROGRAMA               PIC  X(008).
      *
               05  GFCT0M-APLICACAO.
                   10 GFCT0M-TEXTO                  PIC  X(075).
      *
               05  GFCT0M-ERRO-SQL REDEFINES GFCT0M-APLICACAO.
                   10  GFCT0M-NOME-TAB              PIC  X(018).
                   10  GFCT0M-COMANDO-SQL           PIC  X(010).
                   10  GFCT0M-STACODE               PIC  X(004).
                   10  GFCT0M-SQLCODE               REDEFINES
                       GFCT0M-STACODE               PIC S9(009) COMP-4.
                   10  GFCT0M-LOCAL                 PIC  X(004).
                   10  GFCT0M-SEGM                  PIC  X(008).
                   10  FILLER                       PIC  X(031).
      *
           03  GFCT0M-ERRO-SQLCA.
               05  GFCT0M-SQLCA-AREA                PIC  X(136).
