***************************** Top of Data ******************************
      *****************************************************************
      * NOME DA INC - I#GFCTKJ                                        *
      * DESCRICAO   - TEXT DE MUNICIPIO                               *
      *               COMUNICACAO WEB X IMS - ENTRADA                 *
      * TAMANHO     - 100                                             *
      * DATA        - 09.02.2006                                      *
      * RESPONSAVEL - RICARDO PEREIRA - PROCWORK                      *
      *****************************************************************
       01  GFCTKJ-ENTRADA.
           03  GFCTKJ-LL                            PIC S9(004) COMP.
           03  GFCTKJ-ZZ                            PIC S9(004) COMP.
           03  GFCTKJ-TRANSACAO                     PIC  X(009).
           03  GFCTKJ-FUNCAO                        PIC  X(008).
           03  GFCTKJ-DADOS-RST.
               05  GFCTKJ-QTDE-OCOR                 PIC  9(003).
               05  GFCTKJ-QTDE-TOT-REG              PIC  9(006).
               05  GFCTKJ-FUNC-BDSCO                PIC  X(007).
               05  GFCTKJ-CMUN-IBGE                 PIC  9(007).
               05  GFCTKJ-FIM                       PIC  X(001).
               05  FILLER                           PIC  X(055).
