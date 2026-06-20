      *****************************************************************
      * NOME DA INC - GFCTWAGR                                        *
      * DESCRICAO   - AUTORIZACAO DE EMISSAO DE EXTRATO ANUAL         *
      *               DE TARIFAS VIA CORREIO - POR AGRUPAMENTO   EXCL.*
      * TAMANHO     - 1010                                            *
      * WAGR-LL     - 1010                                            *
      * DATA        - MAIO / 2009                                     *
      * RESPONSAVEL - HELIO SANTONI - PROCWORK                        *
      *================================================================*
ST2507*               U L T I M A   A L T E R A C A O                *
      *----------------------------------------------------------------*
ST2507*    ANALISTA....:  STEFANINI                                    *
ST2507*    DATA........:  07 / 2025                                    *
ST2507*    OBJETIVO....:  CNPJ ALFANUMERICO                            *
      *================================================================*
      *
       01  WAGR-SAIDA.
           03 WAGR-LL                            PIC S9(004) COMP.
           03 WAGR-ZZ                            PIC S9(004) COMP.
           03 WAGR-TRANSACAO                     PIC  X(009).
           03 WAGR-FUNCAO                        PIC  X(008).
           03 WAGR-DADOS.
              05 WAGR-FUNC-BDSCO                 PIC  X(007).
              05 WAGR-FIM                        PIC  X(001).
           03 WAGR-ERRO                          PIC  9(001).
           03 WAGR-COD-MSG-ERRO                  PIC  9(007).
           03 WAGR-COD-SQL-ERRO                  PIC  9(003).
           03 WAGR-DESC-MSG-ERRO                 PIC  X(079).
           03 WAGR-CAMPOS.
              05 WAGR-HINCL                      PIC  X(026).
              05 WAGR-CAGPTO                     PIC  9(003).
              05 WAGR-CSEGMTO                    PIC  9(003).
              05 WAGR-CAGEN                      PIC  9(005).
              05 WAGR-CPAB                       PIC  9(005).
              05 WAGR-CCNPJ-CPF.
ST2507*          10 WAGR-NUMERO                  PIC  9(009).
ST2507*          10 WAGR-FILIAL                  PIC  9(005).
ST2507           10 WAGR-NUMERO                  PIC  X(009).
ST2507           10 WAGR-FILIAL                  PIC  X(004).
                 10 WAGR-CONTRL                  PIC  9(002).
              05 WAGR-JUSTIF                     PIC  X(254).
           03 WAGR-FILLER                        PIC  X(579).
