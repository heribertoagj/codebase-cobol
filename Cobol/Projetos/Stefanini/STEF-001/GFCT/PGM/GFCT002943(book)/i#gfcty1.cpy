      *****************************************************************
      * NOME DA INC - I#GFCTY1                                        *
      * DESCRICAO   - LISTA DE TARIFAS SUSPENSAS PARA LIBERACAOPOR AG *
      *               COMUNICACAO IMS X WEB        -       SAIDA      *
      * TAMANHO     - 1010                                            *
      * FIXO        - 296                                             *
      * OCORRENCIA  - 119                                             *
      * VARIAVEL    - 714                                             *
      * DATA        - 01/2007                                         *
      * RESPONSAVEL - EVANISE - PROCWORK - TEL 4152-4352 R.211        *
      *================================================================*
ST2507*               U L T I M A   A L T E R A C A O                *
      *----------------------------------------------------------------*
ST2507*    ANALISTA....:  STEFANINI                                    *
ST2507*    DATA........:  07 / 2025                                    *
ST2507*    OBJETIVO....:  CNPJ ALFANUMERICO                            *
      *================================================================*
      *
       01  GFCTY1-SAIDA.
           03  GFCTY1-LL                            PIC S9(004) COMP.
           03  GFCTY1-ZZ                            PIC S9(004) COMP.
           03  GFCTY1-TRANSACAO                     PIC  X(009).
           03  GFCTY1-FUNCAO                        PIC  X(008).
           03  GFCTY1-FILTRO.
               05 GFCTY1-AGPTO                      PIC  9(003).
               05 GFCTY1-TARIFA                     PIC  9(005).
           03 GFCTY1-PONTEIRO.
               05 GFCTY1-PONT-CSEQ-AGPTO            PIC  9(009).
               05 GFCTY1-PONT-TARIFA                PIC  9(005).
               05 GFCTY1-PONT-AGENCIA               PIC  9(005).
               05 GFCTY1-PONT-POSTO                 PIC  9(005).
ST2507*        05 GFCTY1-PONT-CNPJ-CPF              PIC  9(009).
ST2507*        05 GFCTY1-PONT-FILIAL                PIC  9(005).
ST2507         05 GFCTY1-PONT-CNPJ-CPF              PIC  X(009).
ST2507         05 GFCTY1-PONT-FILIAL                PIC  X(004).
               05 GFCTY1-PONT-CONTROLE              PIC  9(002).
               05 GFCTY1-PONT-COD-MUNIC             PIC  9(007).
               05 GFCTY1-PONT-UF                    PIC  X(002).
               05 GFCTY1-PONT-DATA-SUSP             PIC  X(010).
           03  GFCTY1-QTDE-OCOR                     PIC  9(003).
           03  GFCTY1-QTDE-TOT-REG                  PIC  9(006).
           03  GFCTY1-FUNC-BDSCO                    PIC  X(007).
           03  GFCTY1-ERRO                          PIC  9(001).
           03  GFCTY1-COD-MSG-ERRO                  PIC  9(007).
           03  GFCTY1-COD-SQL-ERRO                  PIC  9(003).
           03  GFCTY1-DESC-MSG-ERRO                 PIC  X(079).
           03  GFCTY1-FIM                           PIC  X(001).
ST2507*    03  FILLER                               PIC  X(101).
ST2507     03  FILLER                               PIC  X(102).
           03  GFCTY1-DADOS.
               05  FILLER                           OCCURS 06.
                   07  GFCTY1-CSEQ-AGPTO            PIC  9(009).
                   07  GFCTY1-COD-TARIFA            PIC  9(005).
                   07  GFCTY1-DESC-TARIFA           PIC  X(020).
                   07  GFCTY1-AGENCIA               PIC  9(005).
                   07  GFCTY1-POSTO                 PIC  9(005).
                   07  GFCTY1-CNPJ-CPF              PIC  9(009).
                   07  GFCTY1-FILIAL                PIC  9(005).
                   07  GFCTY1-CONTROLE              PIC  9(002).
                   07  GFCTY1-COD-MUNIC             PIC  9(007).
                   07  GFCTY1-UF                    PIC  X(002).
                   07  GFCTY1-DESCRICAO             PIC  X(020).
                   07  GFCTY1-DATA-FIM-SUSP         PIC  X(010).
                   07  GFCTY1-DATA-COBR             PIC  X(010).
                   07  GFCTY1-DATA-INIC-SUSP        PIC  X(010).
