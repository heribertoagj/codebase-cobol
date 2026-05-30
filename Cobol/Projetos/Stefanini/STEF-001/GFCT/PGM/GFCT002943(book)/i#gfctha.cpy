      *****************************************************************
      * NOME DA INC - I#GFCTHA                                        *
      * DESCRICAO   - TEXT DE NOME DE CLIENTE                         *
      *               COMUNICACAO IMS X WEB - SAIDA                   *
      * TAMANHO     - 1010                                            *
      * DATA        - 15.05.2008                                      *
      * RESPONSAVEL - RICARDO PEREIRA - PROCWORK - TEL 4506-2345      *
      *================================================================*
ST2507*               U L T I M A   A L T E R A C A O                *
      *----------------------------------------------------------------*
ST2507*    ANALISTA....:  STEFANINI                                    *
ST2507*    DATA........:  07 / 2025                                    *
ST2507*    OBJETIVO....:  CNPJ ALFANUMERICO                            *
      *================================================================*
      *
       01  GFCTHA-SAIDA.
           03  GFCTHA-LL                            PIC S9(004) COMP.
           03  GFCTHA-ZZ                            PIC S9(004) COMP.
           03  GFCTHA-TRANSACAO                     PIC  X(009).
           03  GFCTHA-FUNCAO                        PIC  X(008).
           03  GFCTHA-FUNC-BDSCO                    PIC  X(007).
           03  GFCTHA-COD-DEPDC                     PIC  9(005).
           03  GFCTHA-CCTA-CLI                      PIC  9(007).
           03  GFCTHA-ERRO                          PIC  9(001).
           03  GFCTHA-COD-MSG-ERRO                  PIC  9(007).
           03  GFCTHA-COD-SQL-ERRO                  PIC  9(003).
           03  GFCTHA-DESC-MSG-ERRO                 PIC  X(079).
           03  GFCTHA-FIM                           PIC  X(001).
           03  FILLER                               PIC  X(791).
           03  GFCTHA-DADOS.
               05  GFCTHA-NOME-CLIENTE              PIC  X(070).
               05  GFCTHA-COD-TIPO-CTA              PIC  9(002).
               05  GFCTHA-COD-TIPO-PSSOA            PIC  X(001).
ST2507*        05  GFCTHA-COD-CGC-CPF               PIC  9(009).
ST2507*        05  GFCTHA-COD-FILIAL-CGC            PIC  9(004).
ST2507         05  GFCTHA-COD-CGC-CPF               PIC  X(009).
ST2507         05  GFCTHA-COD-FILIAL-CGC            PIC  X(004).
               05  GFCTHA-COD-CONTROLE-CPF-CGC      PIC  9(002).
