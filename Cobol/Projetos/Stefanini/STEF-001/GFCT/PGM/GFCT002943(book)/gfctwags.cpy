      *****************************************************************
      * NOME DA INC - GFCTWAGS                                        *
      * DESCRICAO   - AUTORIZACAO DE EMISSAO DE EXTRATO ANUAL         *
      *               DE TARIFAS VIA CORREIO - POR AGRUPAMENTO        *
      *                                                     CONS.DET. *
      * WAGS-LL     - 400                                             *
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
       01  WAGS-ENTRADA.
           03 WAGS-LL                            PIC S9(004) COMP.
           03 WAGS-ZZ                            PIC S9(004) COMP.
           03 WAGS-TRANSACAO                     PIC  X(009).
           03 WAGS-FUNCAO                        PIC  X(008).
           03 WAGS-DADOS.
              05 WAGS-FUNC-BDSCO                 PIC  X(007).
              05 WAGS-FIM                        PIC  X(001).
           03 WAGS-CAMPOS.
              05 WAGS-HINCL                      PIC  X(026).
              05 WAGS-CAGPTO                     PIC  9(003).
              05 WAGS-CSEGMTO                    PIC  9(003).
              05 WAGS-CAGEN                      PIC  9(005).
              05 WAGS-CPAB                       PIC  9(005).
              05 WAGS-CCNPJ-CPF.
ST2507*          10 WAGS-NUMERO                  PIC  9(009).
ST2507*          10 WAGS-FILIAL                  PIC  9(005).
ST2507           10 WAGS-NUMERO                  PIC  X(009).
ST2507           10 WAGS-FILIAL                  PIC  X(004).
                 10 WAGS-CONTRL                  PIC  9(002).
           03 FILLER                             PIC  X(313).
