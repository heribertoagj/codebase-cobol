      ******************************************************************
      * NOME DA INC - I#GFCTOK                                         *
      * DESCRICAO   - COMBO TIPO AGRUPAMENTO - FLEXIBILIZACAO          *
      *               COMUNICACAO WEB X IMS - ENTRADA                  *
      * TAMANHO     - 100                                              *
      * DATA        - 01/04/2006                                       *
      * RESPONSAVEL - RICARDO PEREIRA - PROCWORK - TEL 41524352        *
      ******************************************************************
      *
       01  GFCTOK-ENTRADA.
           03  GFCTOK-LL                            PIC S9(004) COMP.
           03  GFCTOK-ZZ                            PIC S9(004) COMP.
           03  GFCTOK-TRANSACAO                     PIC  X(009).
           03  GFCTOK-FUNCAO                        PIC  X(008).
           03  GFCTOK-DADOS-RST.
               05  GFCTOK-FILTRO                    PIC  9(003).
               05  GFCTOK-QTDE-OCOR                 PIC  9(003).
               05  GFCTOK-QTDE-TOT-REG              PIC  9(006).
               05  GFCTOK-FUNC-BDSCO                PIC  X(007).
               05  GFCTOK-PONTEIRO                  PIC  9(003).
               05  GFCTOK-FIM                       PIC  X(001).
INC            05  GFCTOK-COD-PERFIL                PIC  X(008).
               05  FILLER                           PIC  X(048).
