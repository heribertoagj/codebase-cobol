      *****************************************************************
      * NOME DA INC - GFCTWAGP                                        *
      * DESCRICAO   - AUTORIZACAO DE EMISSAO DE EXTRATO ANUAL         *
      *               DE TARIFAS VIA CORREIO - POR AGRUPAMENTO   INCL.*
      * TAMANHO     - 1010                                            *
      * WAGP-LL     - 1010                                            *
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
       01  WAGP-SAIDA.
           03 WAGP-LL                            PIC S9(004) COMP.
           03 WAGP-ZZ                            PIC S9(004) COMP.
           03 WAGP-TRANSACAO                     PIC  X(009).
           03 WAGP-FUNCAO                        PIC  X(008).
           03 WAGP-DADOS.
              05 WAGP-FUNC-BDSCO                 PIC  X(007).
              05 WAGP-FIM                        PIC  X(001).
           03 WAGP-ERRO                          PIC  9(001).
           03 WAGP-COD-MSG-ERRO                  PIC  9(007).
           03 WAGP-COD-SQL-ERRO                  PIC  9(003).
           03 WAGP-DESC-MSG-ERRO                 PIC  X(079).
           03 WAGP-CAMPOS.
              05 WAGP-CAGPTO                     PIC  9(003).
              05 WAGP-CSEGMTO                    PIC  9(003).
              05 WAGP-CAGEN                      PIC  9(005).
              05 WAGP-CPAB                       PIC  9(005).
              05 WAGP-CNPJ-CPF.
ST2507*          10 WAGP-NUMERO                  PIC  9(009).
ST2507*          10 WAGP-FILIAL                  PIC  9(005).
ST2507           10 WAGP-NUMERO                  PIC  X(009).
ST2507           10 WAGP-FILIAL                  PIC  X(004).
                 10 WAGP-CONTRL                  PIC  9(002).
              05 WAGP-JUSTIF                     PIC  X(254).
           03 WAGP-FILLER                        PIC  X(605).
