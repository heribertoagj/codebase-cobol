      *****************************************************************
      * NOME DA INC - GFCTWAG3                                        *
      * DESCRICAO   - CANCELAMENTO DE EMISSAO DE EXTRATO ANUAL        *
      *               DE TARIFAS VIA CORREIO - POR AGRUPAMENTO   INCL.*
      * TAMANHO     - 1010                                            *
      * WAG3-LL     - 1010                                            *
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
       01  WAG3-SAIDA.
           03 WAG3-LL                            PIC S9(004) COMP.
           03 WAG3-ZZ                            PIC S9(004) COMP.
           03 WAG3-TRANSACAO                     PIC  X(009).
           03 WAG3-FUNCAO                        PIC  X(008).
           03 WAG3-DADOS.
              05 WAG3-FUNC-BDSCO                 PIC  X(007).
              05 WAG3-FIM                        PIC  X(001).
           03 WAG3-ERRO                          PIC  9(001).
           03 WAG3-COD-MSG-ERRO                  PIC  9(007).
           03 WAG3-COD-SQL-ERRO                  PIC  9(003).
           03 WAG3-DESC-MSG-ERRO                 PIC  X(079).
           03 WAG3-CAMPOS.
              05 WAG3-CAGPTO                     PIC  9(003).
              05 WAG3-CSEGMTO                    PIC  9(003).
              05 WAG3-CAGEN                      PIC  9(005).
              05 WAG3-CPAB                       PIC  9(005).
              05 WAG3-CNPJ-CPF.
ST2507*          10 WAG3-NUMERO                  PIC  9(009).
ST2507*          10 WAG3-FILIAL                  PIC  9(005).
ST2507           10 WAG3-NUMERO                  PIC  X(009).
ST2507           10 WAG3-FILIAL                  PIC  X(004).
                 10 WAG3-CONTRL                  PIC  9(002).
              05 WAG3-JUSTIF                     PIC  X(254).
           03 WAG3-FILLER                        PIC  X(605).
