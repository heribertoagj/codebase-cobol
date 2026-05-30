      *****************************************************************
      * NOME DA INC - GFCTWAGT                                        *
      * DESCRICAO   - AUTORIZACAO DE EMISSAO DE EXTRATO ANUAL         *
      *               DE TARIFAS VIA CORREIO - POR AGRUPAMENTO        *
      *                                                     CONS.DET. *
      * TAMANHO     - 1010                                            *
      * WAGT-LL     - 1010                                            *
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
       01  WAGT-SAIDA.
           03 WAGT-LL                            PIC S9(004) COMP.
           03 WAGT-ZZ                            PIC S9(004) COMP.
           03 WAGT-TRANSACAO                     PIC  X(009).
           03 WAGT-FUNCAO                        PIC  X(008).
           03 WAGT-DADOS.
              05 WAGT-FUNC-BDSCO                 PIC  X(007).
              05 WAGT-FIM                        PIC  X(001).
           03 WAGT-ERRO                          PIC  9(001).
           03 WAGT-COD-MSG-ERRO                  PIC  9(007).
           03 WAGT-COD-SQL-ERRO                  PIC  9(003).
           03 WAGT-DESC-MSG-ERRO                 PIC  X(079).
           03 WAGT-CAMPOS.
              05 WAGT-HINCL                      PIC  X(026).
              05 WAGT-CAGPTO                     PIC  9(003).
              05 WAGT-CSEGMTO                    PIC  9(003).
              05 WAGT-NM-SEGMTO                  PIC  X(030).
              05 WAGT-CAGEN                      PIC  9(005).
              05 WAGT-CPAB                       PIC  9(005).
              05 WAGT-NM-PAB                     PIC  X(030).
              05 WAGT-CNPJ-CPF.
ST2507*          10 WAGT-NUMERO                  PIC  9(009).
ST2507*          10 WAGT-FILIAL                  PIC  9(005).
ST2507           10 WAGT-NUMERO                  PIC  X(009).
ST2507           10 WAGT-FILIAL                  PIC  X(004).
                 10 WAGT-CONTRL                  PIC  9(002).
              05 WAGT-NM-CLIE                    PIC  X(030).
              05 WAGT-CFUNC-INCL                 PIC  9(009).
              05 WAGT-NFUNC-INCL                 PIC  X(030).
              05 WAGT-DATA-INCL                  PIC  X(010).
              05 WAGT-HORA-INCL                  PIC  X(008).
              05 WAGT-JUSTIF-INCL                PIC  X(254).
              05 WAGT-CFUNC-EXCL                 PIC  9(009).
              05 WAGT-NFUNC-EXCL                 PIC  X(030).
              05 WAGT-DATA-EXCL                  PIC  X(010).
              05 WAGT-HORA-EXCL                  PIC  X(008).
              05 WAGT-JUSTIF-EXCL                PIC  X(254).
           03 WAGT-FILLER                        PIC  X(121).
