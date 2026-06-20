      *****************************************************************
      * NOME DA INC - GFCTWAG8                                        *
      * DESCRICAO   - CANCELAMENTO DE EMISSAO DE EXTRATO ANUAL        *
      *               DE TARIFAS VIA CORREIO - POR AGRUPAMENTO        *
      *                                                     CONS.DET. *
      * WAG8-LL     - 400                                             *
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
       01  WAG8-ENTRADA.
           03 WAG8-LL                            PIC S9(004) COMP.
           03 WAG8-ZZ                            PIC S9(004) COMP.
           03 WAG8-TRANSACAO                     PIC  X(009).
           03 WAG8-FUNCAO                        PIC  X(008).
           03 WAG8-DADOS.
              05 WAG8-FUNC-BDSCO                 PIC  X(007).
              05 WAG8-FIM                        PIC  X(001).
           03 WAG8-CAMPOS.
              05 WAG8-HINCL                      PIC  X(026).
              05 WAG8-CAGPTO                     PIC  9(003).
              05 WAG8-CSEGMTO                    PIC  9(003).
              05 WAG8-CAGEN                      PIC  9(005).
              05 WAG8-CPAB                       PIC  9(005).
              05 WAG8-CCNPJ-CPF.
ST2507*          10 WAG8-NUMERO                  PIC  9(009).
ST2507*          10 WAG8-FILIAL                  PIC  9(005).
ST2507           10 WAG8-NUMERO                  PIC  X(009).
ST2507           10 WAG8-FILIAL                  PIC  X(004).
                 10 WAG8-CONTRL                  PIC  9(002).
           03 FILLER                             PIC  X(314).
