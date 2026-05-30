      *****************************************************************
      * NOME DA INC - I#GFCTOP                                        *
      * DESCRICAO   - COMBO DE TARIFA COM FLEXIBILIZACAO              *
      *               COMUNICACAO IMS X WEB - SAIDA                   *
      * TAMANHO     - 1010                                            *
      * DATA        - 04.04.2006                                      *
      * RESPONSAVEL - RICARDO PEREIRA - PROCWORK - TEL 41524352 R 208 *
      *****************************************************************
      *
       01  GFCTOP-SAIDA.
           03  GFCTOP-LL                            PIC S9(004) COMP.
           03  GFCTOP-ZZ                            PIC S9(004) COMP.
           03  GFCTOP-TRANSACAO                     PIC  X(009).
           03  GFCTOP-FUNCAO                        PIC  X(008).
           03  GFCTOP-FILTRO.
               05  GFCTOP-CAGPTO-CTA                PIC  9(003).
               05  GFCTOP-ARGUMENTOS.
                   07  GFCTOP-ARG1                  PIC  9(009).
                   07  GFCTOP-ARG2                  PIC  9(005).
                   07  GFCTOP-ARG3                  PIC  X(002).
                   07  GFCTOP-ARG4                  PIC  9(003).
                   07  GFCTOP-ARG5                  PIC  X(001).
                   07  GFCTOP-ARG6                  PIC  9(005).
                   07  GFCTOP-ARG7                  PIC  9(007).
                   07  GFCTOP-ARG8                  PIC  9(003).
                   07  GFCTOP-ARG9                  PIC  9(003).
           03  GFCTOP-QTDE-OCOR                     PIC  9(007).
           03  GFCTOP-QTDE-TOT-REG                  PIC  9(009).
           03  GFCTOP-FUNC-BDSCO                    PIC  X(007).
           03  GFCTOP-PONTEIRO.
               05  GFCTOP-PONTEIRO-TARIFA           PIC  9(005).
           03  GFCTOP-ERRO                          PIC  9(001).
           03  GFCTOP-COD-MSG-ERRO                  PIC  9(007).
           03  GFCTOP-COD-SQL-ERRO                  PIC  9(003).
           03  GFCTOP-DESC-MSG-ERRO                 PIC  X(079).
           03  GFCTOP-FIM                           PIC  X(001).
090708     03  FILLER                               PIC  X(029).
           03  GFCTOP-DADOS.
               05  FILLER                           OCCURS 8.
                   07  GFCTOP-COD-TARIFA            PIC  9(005).
090708             07  GFCTOP-DESC-TARIFA           PIC  X(090).
090709             07  GFCTOP-PRMSS-AGNDA           PIC  9(001).
090708             07  GFCTOP-PRMSS-VALOR           PIC  9(001).
090708             07  GFCTOP-PRMSS-PERCENT         PIC  9(001).
090708             07  GFCTOP-PRMSS-FRANQ           PIC  9(001).
090708             07  GFCTOP-PRMSS-PERDC           PIC  9(001).
