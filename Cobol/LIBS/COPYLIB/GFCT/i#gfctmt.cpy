      ******************************************************************
      * NOME DA INC - I#GFCTMT                                         *
      * DESCRICAO   - COMBO TIPO AGRUPAMENTO                           *
      *               COMUNICACAO WEB X IMS - ENTRADA                  *
      * TAMANHO     - 100                                              *
      * DATA        - 07/03/2006                                       *
      * RESPONSAVEL - RICARDO PEREIRA - PROCWORK - TEL 41524352        *
      ******************************************************************
      *
       01  GFCTMT-ENTRADA.
           03  GFCTMT-LL                            PIC S9(004) COMP.
           03  GFCTMT-ZZ                            PIC S9(004) COMP.
           03  GFCTMT-TRANSACAO                     PIC  X(009).
           03  GFCTMT-FUNCAO                        PIC  X(008).
           03  GFCTMT-DADOS-RST.
               05  GFCTMT-FILTRO                    PIC  9(003).
               05  GFCTMT-QTDE-OCOR                 PIC  9(003).
               05  GFCTMT-QTDE-TOT-REG              PIC  9(006).
               05  GFCTMT-FUNC-BDSCO                PIC  X(007).
               05  GFCTMT-TERMINAL                  PIC  X(008).
               05  GFCTMT-PONTEIRO                  PIC  9(003).
               05  GFCTMT-FIM                       PIC  X(001).
INC            05  GFCTMT-COD-PERFIL                PIC  X(008).
               05  FILLER                           PIC  X(040).
