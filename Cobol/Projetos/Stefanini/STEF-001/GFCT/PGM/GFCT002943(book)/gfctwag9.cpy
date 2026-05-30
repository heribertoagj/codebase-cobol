      *****************************************************************
      * NOME DA INC - GFCTWAG9                                        *
      * DESCRICAO   - CANCELAMENTO DE EMISSAO DE EXTRATO ANUAL        *
      *               DE TARIFAS VIA CORREIO - POR AGRUPAMENTO        *
      *                                                     CONS.DET. *
      * TAMANHO     - 1010                                            *
      * WAG9-LL     - 1010                                            *
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
       01  WAG9-SAIDA.
           03 WAG9-LL                            PIC S9(004) COMP.
           03 WAG9-ZZ                            PIC S9(004) COMP.
           03 WAG9-TRANSACAO                     PIC  X(009).
           03 WAG9-FUNCAO                        PIC  X(008).
           03 WAG9-DADOS.
              05 WAG9-FUNC-BDSCO                 PIC  X(007).
              05 WAG9-FIM                        PIC  X(001).
           03 WAG9-ERRO                          PIC  9(001).
           03 WAG9-COD-MSG-ERRO                  PIC  9(007).
           03 WAG9-COD-SQL-ERRO                  PIC  9(003).
           03 WAG9-DESC-MSG-ERRO                 PIC  X(079).
           03 WAG9-CAMPOS.
              05 WAG9-HINCL                      PIC  X(026).
              05 WAG9-CAGPTO                     PIC  9(003).
              05 WAG9-CSEGMTO                    PIC  9(003).
              05 WAG9-NM-SEGMTO                  PIC  X(030).
              05 WAG9-CAGEN                      PIC  9(005).
              05 WAG9-CPAB                       PIC  9(005).
              05 WAG9-NM-PAB                     PIC  X(030).
              05 WAG9-CNPJ-CPF.
ST2507*          10 WAG9-NUMERO                  PIC  9(009).
ST2507*          10 WAG9-FILIAL                  PIC  9(005).
ST2507           10 WAG9-NUMERO                  PIC  X(009).
ST2507           10 WAG9-FILIAL                  PIC  X(004).
                 10 WAG9-CONTRL                  PIC  9(002).
              05 WAG9-NM-CLIE                    PIC  X(030).
              05 WAG9-CFUNC-INCL                 PIC  9(009).
              05 WAG9-NFUNC-INCL                 PIC  X(030).
              05 WAG9-DATA-INCL                  PIC  X(010).
              05 WAG9-HORA-INCL                  PIC  X(008).
              05 WAG9-JUSTIF-INCL                PIC  X(254).
              05 WAG9-CFUNC-EXCL                 PIC  9(009).
              05 WAG9-NFUNC-EXCL                 PIC  X(030).
              05 WAG9-DATA-EXCL                  PIC  X(010).
              05 WAG9-HORA-EXCL                  PIC  X(008).
              05 WAG9-JUSTIF-EXCL                PIC  X(254).
           03 WAG9-FILLER                        PIC  X(121).
