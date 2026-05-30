***************************** Top of Data ******************************
      *****************************************************************
      * NOME DA INC - I#GFCTKL                                        *
      * DESCRICAO   - COMBO DE UF                                     *
      *               COMUNICACAO IMS X WEB - ENTRADA                 *
      * TAMANHO     - 100                                             *
      * DATA        - 09.02.2006                                      *
      * RESPONSAVEL - RICARDO PEREIRA - PROCWORK - TEL.: 4152-4352    *
      *****************************************************************
      *
       01  GFCTKL-ENTRADA.
           03  GFCTKL-LL                            PIC S9(004) COMP.
           03  GFCTKL-ZZ                            PIC S9(004) COMP.
           03  GFCTKL-TRANSACAO                     PIC  X(009).
           03  GFCTKL-FUNCAO                        PIC  X(008).
           03  GFCTKL-DADOS-RST.
               05  GFCTKL-FILTRO.
                   07 GFCTKL-FILTRO-UF              PIC  X(002).
               05  GFCTKL-QTDE-OCOR                 PIC  9(003).
               05  GFCTKL-QTDE-TOT-REG              PIC  9(006).
               05  GFCTKL-FUNC-BDSCO                PIC  X(007).
               05  GFCTKL-PONTEIRO.
                   07 GFCTKL-PONTEIRO-UF            PIC  X(002).
               05  GFCTKL-FIM                       PIC  X(001).
               05  FILLER                           PIC  X(058).
