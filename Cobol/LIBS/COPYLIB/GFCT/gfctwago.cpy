      *****************************************************************
      * NOME DA INC - GFCTWAGO                                        *
      * DESCRICAO   - AUTORIZACAO DE EMISSAO DE EXTRATO ANUAL         *
      *               DE TARIFAS VIA CORREIO - POR AGRUPAMENTO   INCL.*
      *                                                               *
      * WAGO-LL     - 630                                             *
      * DATA        - MAIO / 2009                                     *
      * RESPONSAVEL - HELIO SANTONI - PROCWORK                        *
      *****************************************************************
      *
       01  WAGO-ENTRADA.
           03 WAGO-LL                            PIC S9(004) COMP.
           03 WAGO-ZZ                            PIC S9(004) COMP.
           03 WAGO-TRANSACAO                     PIC  X(009).
           03 WAGO-FUNCAO                        PIC  X(008).
           03 WAGO-DADOS.
              05 WAGO-FUNC-BDSCO                 PIC  X(007).
              05 WAGO-FIM                        PIC  X(001).
           03 WAGO-CAMPOS.
              05 WAGO-CAGPTO                     PIC  9(003).
              05 WAGO-CSEGMTO                    PIC  9(003).
              05 WAGO-CAGEN                      PIC  9(005).
              05 WAGO-CPAB                       PIC  9(005).
              05 WAGO-CCNPJ-CPF.
                 10 WAGO-NUMERO                  PIC  9(009).
                 10 WAGO-FILIAL                  PIC  9(005).
                 10 WAGO-CONTRL                  PIC  9(002).
              05 WAGO-JUSTIF                     PIC  X(254).
           03 FILLER                             PIC  X(315).
