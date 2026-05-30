      *****************************************************************
      * NOME DA INC - I#GFCTP4                                        *
      * DESCRICAO   - TEXT DE NOME DO MUNICIPIO COM AGRUPAMENTO       *
      *               COMUNICACAO WEB X IMS - ENTRADA                 *
      * TAMANHO     - 100                                             *
      * DATA        - 04.04.2006                                      *
      * RESPONSAVEL - RICARDO PEREIRA - PROCWORK                      *
      *****************************************************************
       01  GFCTP4-ENTRADA.
           03  GFCTP4-LL                            PIC S9(004) COMP.
           03  GFCTP4-ZZ                            PIC S9(004) COMP.
           03  GFCTP4-TRANSACAO                     PIC  X(009).
           03  GFCTP4-FUNCAO                        PIC  X(008).
           03  GFCTP4-DADOS-RST.
               05  GFCTP4-FILTRO.
                   07 GFCTP4-FILTRO-AGRUP           PIC  9(003).
                   07 GFCTP4-FILTRO-CMUN            PIC  9(007).
               05  GFCTP4-QTDE-OCOR                 PIC  9(007).
               05  GFCTP4-QTDE-TOT-REG              PIC  9(009).
               05  GFCTP4-FUNC-BDSCO                PIC  X(007).
               05  GFCTP4-FIM                       PIC  X(001).
               05  FILLER                           PIC  X(045).
