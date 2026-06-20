      *****************************************************************
      * NOME DA INC - GFCTWAG4                                        *
      * DESCRICAO   - CANCELAMENTO DE EMISSAO DE EXTRATO ANUAL        *
      *               DE TARIFAS VIA CORREIO - POR AGRUPAMENTO   EXCL.*
      *                                                               *
      * WAG4-LL     - 650                                             *
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
       01  WAG4-ENTRADA.
           03 WAG4-LL                            PIC S9(004) COMP.
           03 WAG4-ZZ                            PIC S9(004) COMP.
           03 WAG4-TRANSACAO                     PIC  X(009).
           03 WAG4-FUNCAO                        PIC  X(008).
           03 WAG4-DADOS.
              05 WAG4-FUNC-BDSCO                 PIC  X(007).
              05 WAG4-FIM                        PIC  X(001).
           03 WAG4-CAMPOS.
              05 WAG4-HINCL                      PIC  X(026).
              05 WAG4-CAGPTO                     PIC  9(003).
              05 WAG4-CSEGMTO                    PIC  9(003).
              05 WAG4-CAGEN                      PIC  9(005).
              05 WAG4-CPAB                       PIC  9(005).
              05 WAG4-CCNPJ-CPF.
ST2507*          10 WAG4-NUMERO                  PIC  9(009).
ST2507*          10 WAG4-FILIAL                  PIC  9(005).
ST2507           10 WAG4-NUMERO                  PIC  X(009).
ST2507           10 WAG4-FILIAL                  PIC  X(004).
                 10 WAG4-CONTRL                  PIC  9(002).
              05 WAG4-JUSTIF                     PIC  X(254).
           03 FILLER                             PIC  X(309).
