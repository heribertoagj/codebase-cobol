      ******************************************************************
      * NOME DA INC - I#GFCTE1                                         *
      * DESCRICAO   - ADESAO INDIVIDUAL - CONSULTA DETALHADA - GFCT0131*
      *               COMUNICACAO WEB X IMS        - SAIDA             *
      * TAMANHO     - 1010                                             *
      * DATA        - 07.2008                                          *
      * RESPONSAVEL - RICARDO PEREIRA DA SILVA - PROCWORK - GP 50.     *
      *================================================================*
ST2507*               U L T I M A   A L T E R A C A O                *
      *----------------------------------------------------------------*
ST2507*    ANALISTA....:  STEFANINI                                    *
ST2507*    DATA........:  07 / 2025                                    *
ST2507*    OBJETIVO....:  CNPJ ALFANUMERICO                            *
      *================================================================*
      *
       01  GFCTE1-SAIDA.
           03  GFCTE1-LL                            PIC S9(004) COMP.
           03  GFCTE1-ZZ                            PIC S9(004) COMP.
           03  GFCTE1-TRANSACAO                     PIC  X(009).
           03  GFCTE1-FUNCAO                        PIC  X(008).
           03  GFCTE1-QTDE-OCOR                     PIC  9(003).
           03  GFCTE1-QTDE-TOT-REG                  PIC  9(006).
           03  GFCTE1-FUNC-BDSCO                    PIC  X(007).
           03  GFCTE1-ERRO                          PIC  9(001).
           03  GFCTE1-COD-MSG-ERRO                  PIC  9(007).
           03  GFCTE1-COD-SQL-ERRO                  PIC  9(003).
           03  GFCTE1-DESC-MSG-ERRO                 PIC  X(079).
           03  GFCTE1-FIM                           PIC  X(001).
           03  GFCTE1-DADOS.
               05  GFCTE1-AGENCIA                   PIC  9(005).
               05  GFCTE1-NOME-AGENCIA              PIC  X(040).
               05  GFCTE1-CONTA                     PIC  9(007).
               05  GFCTE1-COD-TARIFA                PIC  9(005).
               05  GFCTE1-DESCR-TARIFA              PIC  X(040).
               05  GFCTE1-HINCL-REG                 PIC  X(026).
               05  GFCTE1-SITUACAO-ADESAO           PIC  X(009).
ST2507*        05  GFCTE1-CPF-CNPJ                  PIC  9(009).
ST2507*        05  GFCTE1-FILIAL                    PIC  9(004).
ST2507         05  GFCTE1-CPF-CNPJ                  PIC  X(009).
ST2507         05  GFCTE1-FILIAL                    PIC  X(004).
               05  GFCTE1-CONTROLE                  PIC  X(002).
               05  GFCTE1-NOME-CLI                  PIC  X(040).
               05  GFCTE1-CANAL-INCLUSAO            PIC  X(040).
               05  GFCTE1-CANAL-CANCEL              PIC  X(040).
               05  GFCTE1-COD-FUNC-INCL             PIC  X(007).
               05  GFCTE1-DESC-FUNC-INCL            PIC  X(040).
               05  GFCTE1-DATA-INCL                 PIC  X(010).
               05  GFCTE1-HORA-INCL                 PIC  X(008).
               05  GFCTE1-COD-FUNC-CANCEL           PIC  X(007).
               05  GFCTE1-DESC-FUNC-CANCEL          PIC  X(040).
               05  GFCTE1-DATA-CANCEL               PIC  X(010).
               05  GFCTE1-HORA-CANCEL               PIC  X(008).
               05  GFCTE1-CD-OPERADORA              PIC  9(009).
               05  GFCTE1-DS-OPERADORA              PIC  X(040).
               05  GFCTE1-CD-DDD                    PIC  9(002).
               05  GFCTE1-NR-TELEFONE               PIC  9(011).
               05  FILLER                           PIC  X(423).
