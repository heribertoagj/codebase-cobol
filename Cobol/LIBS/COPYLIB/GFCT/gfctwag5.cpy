      *****************************************************************
      * NOME DA INC - GFCTWAG5                                        *
      * DESCRICAO   - CANCELAMENTO DE EMISSAO DE EXTRATO ANUAL        *
      *               DE TARIFAS VIA CORREIO - POR AGRUPAMENTO   EXCL.*
      * TAMANHO     - 1010                                            *
      * WAG5-LL     - 1010                                            *
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
       01  WAG5-SAIDA.
           03 WAG5-LL                            PIC S9(004) COMP.
           03 WAG5-ZZ                            PIC S9(004) COMP.
           03 WAG5-TRANSACAO                     PIC  X(009).
           03 WAG5-FUNCAO                        PIC  X(008).
           03 WAG5-DADOS.
              05 WAG5-FUNC-BDSCO                 PIC  X(007).
              05 WAG5-FIM                        PIC  X(001).
           03 WAG5-ERRO                          PIC  9(001).
           03 WAG5-COD-MSG-ERRO                  PIC  9(007).
           03 WAG5-COD-SQL-ERRO                  PIC  9(003).
           03 WAG5-DESC-MSG-ERRO                 PIC  X(079).
           03 WAG5-CAMPOS.
              05 WAG5-HINCL                      PIC  X(026).
              05 WAG5-CAGPTO                     PIC  9(003).
              05 WAG5-CSEGMTO                    PIC  9(003).
              05 WAG5-CAGEN                      PIC  9(005).
              05 WAG5-CPAB                       PIC  9(005).
              05 WAG5-CNPJ-CPF.
ST2507*          10 WAG5-NUMERO                  PIC  9(009).
ST2507*          10 WAG5-FILIAL                  PIC  9(005).
ST2507           10 WAG5-NUMERO                  PIC  X(009).
ST2507           10 WAG5-FILIAL                  PIC  X(004).
                 10 WAG5-CONTRL                  PIC  9(002).
              05 WAG5-JUSTIF                     PIC  X(254).
           03 WAG5-FILLER                        PIC  X(579).
