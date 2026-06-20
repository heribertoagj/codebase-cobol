      ******************************************************************
      * NOME DA INC - I#GFCTY2                                         *
      * DESCRICAO   - LIBERACAO DE TARIFAS SUSPENSAS POR AGENCIA-CTA   *
      *               COMUNICACAO IMS X WEB      -      ENTRADA        *
      * TAMANHO     - 600                                              *
      * FIXO        - 055                                              *
      * OCORRENCIA  - 109                                              *
      * VARIAVEL    - 545                                              *
      * DATA        - 01/2007                                          *
      * RESPONSAVEL - EVANISE - PROCWORK - TEL 4152-4352 R.211         *
      *================================================================*
ST2507*               U L T I M A   A L T E R A C A O                *
      *----------------------------------------------------------------*
ST2507*    ANALISTA....:  STEFANINI                                    *
ST2507*    DATA........:  07 / 2025                                    *
ST2507*    OBJETIVO....:  CNPJ ALFANUMERICO                            *
      *================================================================*
      *
       01  GFCTY2-ENTRADA.
           03  GFCTY2-LL                            PIC S9(004) COMP.
           03  GFCTY2-ZZ                            PIC S9(004) COMP.
           03  GFCTY2-TRANSACAO                     PIC  X(009).
           03  GFCTY2-FUNCAO                        PIC  X(008).
           03  GFCTY2-DADOS-RST.
               05  GFCTY2-FILTRO.
                   07 GFCTY2-AGPTO                  PIC  9(003).
                   07 GFCTY2-TARIFA                 PIC  9(005).
               05  GFCTY2-QTDE-OCOR                 PIC  9(003).
               05  GFCTY2-QTDE-TOT-REG              PIC  9(006).
               05  GFCTY2-FUNC-BDSCO                PIC  X(007).
               05  GFCTY2-FIM                       PIC  X(001).
               05  FILLER                           PIC  X(009).
               05  GFCTY2-DADOS.
                   07  FILLER                        OCCURS 05.
                       09  GFCTY2-CSEQ-AGPTO         PIC  9(009).
                       09  GFCTY2-COD-TARIFA         PIC  9(005).
                       09  GFCTY2-DESC-TARIFA        PIC  X(020).
                       09  GFCTY2-AGENCIA            PIC  9(005).
                       09  GFCTY2-POSTO              PIC  9(005).
ST2507*                09  GFCTY2-CNPJ-CPF           PIC  9(016).
ST2507                 09  GFCTY2-CNPJ-CPF           PIC  X(015).
                       09  GFCTY2-COD-MUNIC          PIC  9(007).
                       09  GFCTY2-UF                 PIC  X(002).
                       09  GFCTY2-DESCRICAO          PIC  X(020).
                       09  GFCTY2-DATA-FIM-SUSP      PIC  X(010).
                       09  GFCTY2-DATA-COBR          PIC  X(010).
               05  FILLER                            PIC  X(005).
