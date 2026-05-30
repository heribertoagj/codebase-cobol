      *****************************************************************
      * NOME DA INC - GFCTWAGV                                        *
      * DESCRICAO   - AUTORIZACAO DE EMISSAO DE EXTRATO ANUAL         *
      *               DE TARIFAS VIA CORREIO - POR AGRUPAMENTO        *
      *                                                     CONS.LIST.*
      * TAMANHO     - 1010                                            *
      * WAGV-LL     - 1010                                            *
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
       01  WAGV-SAIDA.
           03 WAGV-LL                            PIC S9(004) COMP.
           03 WAGV-ZZ                            PIC S9(004) COMP.
           03 WAGV-TRANSACAO                     PIC  X(009).
           03 WAGV-FUNCAO                        PIC  X(008).
           03 WAGV-DADOS-RST.
              05 WAGV-QTDE-OCOR                  PIC  9(003).
              05 WAGV-QTDE-TOT-REG               PIC  9(006).
              05 WAGV-FUNC-BDSCO                 PIC  X(007).
              05 WAGV-FIM                        PIC  X(001).
              05 WAGV-FILTRO.
                 07 WAGV-FTR-CAGPTO              PIC  9(003).
           03 WAGV-ERRO                          PIC  9(001).
           03 WAGV-COD-MSG-ERRO                  PIC  9(007).
           03 WAGV-COD-SQL-ERRO                  PIC  9(003).
           03 WAGV-DESC-MSG-ERRO                 PIC  X(079).
           03 WAGV-FILLER                        PIC  X(089).
           03 WAGV-OCORR                         OCCURS 05 TIMES.
              05 WAGV-HINCL                      PIC  X(026).
              05 WAGV-CAGPTO                     PIC  9(003).
              05 WAGV-CSEGMTO                    PIC  9(003).
              05 WAGV-NM-SEGMTO                  PIC  X(030).
              05 WAGV-CAGEN                      PIC  9(005).
              05 WAGV-CPAB                       PIC  9(005).
              05 WAGV-NM-PAB                     PIC  X(020).
              05 WAGV-CCNPJ-CPF.
ST2507*          10 WAGV-NUMERO                  PIC  9(009).
ST2507*          10 WAGV-FILIAL                  PIC  9(005).
ST2507           10 WAGV-NUMERO                  PIC  X(009).
ST2507           10 WAGV-FILIAL                  PIC  X(004).
                 10 WAGV-CONTRL                  PIC  9(002).
              05 WAGV-NM-CLIE                    PIC  X(030).
              05 WAGV-DT-SOLC                    PIC  X(010).
              05 WAGV-DT-EXCL                    PIC  X(010).
