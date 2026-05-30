      *****************************************************************
      * NOME DA INC - I#GFCTYB                                        *
      * DESCRICAO   - LISTA DE TARIFAS SUSPENSAS POR AGRUPAMENTO      *
      *               COMUNICACAO IMS X WEB        -       SAIDA      *
      * TAMANHO     - 1010                                            *
      * FIXO        - 415                                             *
      * OCORRENCIA  - 119                                             *
      * VARIAVEL    - 595                                             *
      * DATA        - 01/2007                                         *
      * RESPONSAVEL - VANI NUNES -   PROCWORK - TEL 4152-4352 R.211   *
      *================================================================*
ST2507*               U L T I M A   A L T E R A C A O                *
      *----------------------------------------------------------------*
ST2507*    ANALISTA....:  STEFANINI                                    *
ST2507*    DATA........:  07 / 2025                                    *
ST2507*    OBJETIVO....:  CNPJ ALFANUMERICO                            *
      *================================================================*
      *
       01  GFCTYB-SAIDA.
           03  GFCTYB-LL                            PIC S9(004) COMP.
           03  GFCTYB-ZZ                            PIC S9(004) COMP.
           03  GFCTYB-TRANSACAO                     PIC  X(009).
           03  GFCTYB-FUNCAO                        PIC  X(008).
           03  GFCTYB-FILTRO.
               05 GFCTYB-AGPTO                      PIC  9(003).
               05 GFCTYB-TARIFA                     PIC  9(005).
           03  GFCTYB-QTDE-OCOR                     PIC  9(003).
           03  GFCTYB-QTDE-TOT-REG                  PIC  9(006).
           03  GFCTYB-FUNC-BDSCO                    PIC  X(007).
           03  GFCTYB-ERRO                          PIC  9(001).
           03  GFCTYB-COD-MSG-ERRO                  PIC  9(007).
           03  GFCTYB-COD-SQL-ERRO                  PIC  9(003).
           03  GFCTYB-DESC-MSG-ERRO                 PIC  X(079).
           03  GFCTYB-FIM                           PIC  X(001).
           03  FILLER                               PIC  X(279).
           03  GFCTYB-DADOS.
               05  FILLER                           OCCURS 05.
                   07  GFCTYB-CSEQ-AGPTO            PIC  9(009).
                   07  GFCTYB-COD-TARIFA            PIC  9(005).
                   07  GFCTYB-DESC-TARIFA           PIC  X(020).
                   07  GFCTYB-AGENCIA               PIC  9(005).
                   07  GFCTYB-POSTO                 PIC  9(005).
ST2507*            07  GFCTYB-CNPJ-CPF              PIC  9(009).
ST2507*            07  GFCTYB-FILIAL                PIC  9(005).
ST2507             07  GFCTYB-CNPJ-CPF              PIC  X(009).
ST2507             07  GFCTYB-FILIAL                PIC  X(004).
                   07  GFCTYB-CONTROLE              PIC  9(002).
                   07  GFCTYB-COD-MUNIC             PIC  9(007).
                   07  GFCTYB-UF                    PIC  X(002).
                   07  GFCTYB-DESCRICAO             PIC  X(020).
                   07  GFCTYB-DATA-FIM-SUSP         PIC  X(010).
                   07  GFCTYB-DATA-COBR             PIC  X(010).
                   07  GFCTYB-DATA-INIC-SUSP        PIC  X(010).
ST2507     03  FILLER                               PIC  X(005).
