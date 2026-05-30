      *****************************************************************
      * NOME DA INC - GFCTWAG2                                        *
      * DESCRICAO   - CANCELAMENTO DE EMISSAO DE EXTRATO ANUAL        *
      *               DE TARIFAS VIA CORREIO - POR AGRUPAMENTO   INCL.*
      *                                                               *
      * WAG2-LL     - 630                                             *
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
       01  WAG2-ENTRADA.
           03 WAG2-LL                            PIC S9(004) COMP.
           03 WAG2-ZZ                            PIC S9(004) COMP.
           03 WAG2-TRANSACAO                     PIC  X(009).
           03 WAG2-FUNCAO                        PIC  X(008).
           03 WAG2-DADOS.
              05 WAG2-FUNC-BDSCO                 PIC  X(007).
              05 WAG2-FIM                        PIC  X(001).
           03 WAG2-CAMPOS.
              05 WAG2-CAGPTO                     PIC  9(003).
              05 WAG2-CSEGMTO                    PIC  9(003).
              05 WAG2-CAGEN                      PIC  9(005).
              05 WAG2-CPAB                       PIC  9(005).
              05 WAG2-CCNPJ-CPF.
ST2507*          10 WAG2-NUMERO                  PIC  9(009).
ST2507*          10 WAG2-FILIAL                  PIC  9(005).
ST2507           10 WAG2-NUMERO                  PIC  X(009).
ST2507           10 WAG2-FILIAL                  PIC  X(004).
                 10 WAG2-CONTRL                  PIC  9(002).
              05 WAG2-JUSTIF                     PIC  X(254).
           03 FILLER                             PIC  X(315).
