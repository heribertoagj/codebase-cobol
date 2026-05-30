      *****************************************************************
      * NOME DA INC - I#GFCTWW                                        *
      * DESCRICAO   - TEXT DE NOME DE CLIENTE                         *
      *               COMUNICACAO IMS X WEB - SAIDA                   *
      * TAMANHO     - 1010                                            *
      * DATA        - 15.05.2008                                      *
      * RESPONSAVEL - RICARDO PEREIRA - PROCWORK - TEL 4506-2345      *
      *****************************************************************
      *
       01  GFCTWW-SAIDA.
           03  GFCTWW-LL                            PIC S9(004) COMP.
           03  GFCTWW-ZZ                            PIC S9(004) COMP.
           03  GFCTWW-TRANSACAO                     PIC  X(009).
           03  GFCTWW-FUNCAO                        PIC  X(008).
           03  GFCTWW-FUNC-BDSCO                    PIC  X(007).
           03  GFCTWW-COD-DEPDC                     PIC  9(005).
           03  GFCTWW-CCTA-CLI                      PIC  9(007).
           03  GFCTWW-ERRO                          PIC  9(001).
           03  GFCTWW-COD-MSG-ERRO                  PIC  9(007).
           03  GFCTWW-COD-SQL-ERRO                  PIC  9(003).
           03  GFCTWW-DESC-MSG-ERRO                 PIC  X(079).
           03  GFCTWW-FIM                           PIC  X(001).
           03  FILLER                               PIC  X(806).
           03  GFCTWW-DADOS.
               05  GFCTWW-NOME-CLIENTE              PIC  X(070).
               05  GFCTWW-CTPO-CTA                  PIC  9(002).
      *                01 - CONTA CORRENTE
      *                02 - POUPANCA
      *                03 - POUPANCA PLENA
               05  GFCTWW-CTPO-PSSOA                PIC  X(001).
      *                F - FISICA
      *                J - JURIDICA
