      *****************************************************************
      * NOME DA INC - I#GFCTYA                                        *
      * DESCRICAO   - LIBERACAO DE SUSPENSAO DE TARIFAS P/AGRUPAMENTO *
      *               COMUNICACAO IMS X WEB        -       ENTRADA    *
      * TAMANHO     - 700                                             *
      * FIXO        - 105                                             *
      * OCORRENCIA  - 119                                             *
      * VARIAVEL    - 595                                             *
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
       01  GFCTYA-ENTRADA.
           03  GFCTYA-LL                            PIC S9(004) COMP.
           03  GFCTYA-ZZ                            PIC S9(004) COMP.
           03  GFCTYA-TRANSACAO                     PIC  X(009).
           03  GFCTYA-FUNCAO                        PIC  X(008).
           03  GFCTYA-FILTRO.
               05 GFCTYA-AGPTO                      PIC  9(003).
               05 GFCTYA-TARIFA                     PIC  9(005).
           03  GFCTYA-QTDE-OCOR                     PIC  9(003).
           03  GFCTYA-QTDE-TOT-REG                  PIC  9(006).
           03  GFCTYA-FUNC-BDSCO                    PIC  X(007).
           03  GFCTYA-ERRO                          PIC  9(001).
           03  GFCTYA-FIM                           PIC  X(001).
           03  FILLER                               PIC  X(058).
           03  GFCTYA-DADOS.
               05  FILLER                           OCCURS 05.
                   07  GFCTYA-CSEQ-AGPTO            PIC  9(009).
                   07  GFCTYA-COD-TARIFA            PIC  9(005).
                   07  GFCTYA-DESC-TARIFA           PIC  X(020).
                   07  GFCTYA-AGENCIA               PIC  9(005).
                   07  GFCTYA-POSTO                 PIC  9(005).
ST2507*            07  GFCTYA-CNPJ-CPF              PIC  9(009).
ST2507*            07  GFCTYA-FILIAL                PIC  9(005).
ST2507             07  GFCTYA-CNPJ-CPF              PIC  X(009).
ST2507             07  GFCTYA-FILIAL                PIC  X(004).
                   07  GFCTYA-CONTROLE              PIC  9(002).
                   07  GFCTYA-COD-MUNIC             PIC  9(007).
                   07  GFCTYA-UF                    PIC  X(002).
                   07  GFCTYA-DESCRICAO             PIC  X(020).
                   07  GFCTYA-DATA-FIM-SUSP         PIC  X(010).
                   07  GFCTYA-DATA-COBR             PIC  X(010).
                   07  GFCTYA-DATA-INIC-SUSP        PIC  X(010).
ST2507     03  FILLER                               PIC  X(005).
