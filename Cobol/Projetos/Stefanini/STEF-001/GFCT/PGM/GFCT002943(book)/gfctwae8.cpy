      ******************************************************************
      * NOME DA INC - GFCTWAE8                                         *
      * DESCRICAO   - TEXT DE DADOS DO CLIENTE                         *
      *               COMUNICACAO IMS X WEB - SAIDA                    *
      * TAMANHO     - 1010                                             *
      * DATA        - 28.01.2009                                       *
      * RESPONSAVEL - FRANCISCO LOURENCO - PROCWORK - TEL 4506-2345    *
      *================================================================*
ST2507*               U L T I M A   A L T E R A C A O                  *
      *----------------------------------------------------------------*
ST2507*    ANALISTA....:  STEFANINI                                    *
ST2507*    DATA........:  07 / 2025                                    *
ST2507*    OBJETIVO....:  CNPJ ALFANUMERICO                            *
      *================================================================*
      *
       01  GFCTWAE8-SAIDA.
           03  GFCTWAE8-LL                       PIC S9(004) COMP.
           03  GFCTWAE8-ZZ                       PIC S9(004) COMP.
           03  GFCTWAE8-TRANSACAO                PIC  X(009).
           03  GFCTWAE8-FUNCAO                   PIC  X(008).
           03  GFCTWAE8-FUNC-BDSCO               PIC  X(007).
           03  GFCTWAE8-COD-AGENCIA              PIC  9(005).
           03  GFCTWAE8-COD-CONTA                PIC  9(007).
           03  GFCTWAE8-ERRO                     PIC  9(001).
           03  GFCTWAE8-COD-MSG-ERRO             PIC  9(007).
           03  GFCTWAE8-COD-SQL-ERRO             PIC  9(003).
           03  GFCTWAE8-DESC-MSG-ERRO            PIC  X(079).
           03  GFCTWAE8-FIM                      PIC  X(001).
           03  FILLER                            PIC  X(782).
           03  GFCTWAE8-DADOS.
               05  GFCTWAE8-NOME-CLIENTE         PIC  X(070).
               05  GFCTWAE8-COD-TIPO-CONTA       PIC  9(002).
               05  GFCTWAE8-COD-TIPO-PESSOA      PIC  X(001).
ST2507*        05  GFCTWAE8-COD-CGC-CPF          PIC  9(009).
ST2507*        05  GFCTWAE8-COD-FILIAL-CGC       PIC  9(004).
ST2507         05  GFCTWAE8-COD-CGC-CPF          PIC  X(009).
ST2507         05  GFCTWAE8-COD-FILIAL-CGC       PIC  X(004).
               05  GFCTWAE8-COD-CONTR-CPF-CGC    PIC  9(002).
               05  GFCTWAE8-COD-GRUPO-CTBIL      PIC  9(003).
               05  GFCTWAE8-COD-SUB-GRUPO-CTBIL  PIC  9(003).
               05  GFCTWAE8-COD-SEGMTO-CLI       PIC  9(003).
