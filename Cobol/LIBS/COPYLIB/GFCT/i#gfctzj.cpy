      ******************************************************************
      * NOME DA INC - I#GFCTZJ                                         *
      * DESCRICAO   - RETORNO DIARIO DAS COBRANCAS DE TARIFAS EFETUDAS *
      *               PELO SISTEMA MORA E POUP, PODENDO  SER  FILTRADO *
      *               POR  CONJUNTO DE SERVICO,  TARIFA INDIVIDUAL  OU *
      *               TODAS AS TARIFAS RETORNADAS                      *
      * TAMANHO     - 080                                              *
      * DATA        - 05/2007                                          *
      * RESPONSAVEL - FRANCISCO LOURENCO - PROCWORK - TEL 3068-9902    *
      *================================================================*
ST2507*               U L T I M A   A L T E R A C A O                *
      *----------------------------------------------------------------*
ST2507*    ANALISTA....:  STEFANINI                                    *
ST2507*    DATA........:  07 / 2025                                    *
ST2507*    OBJETIVO....:  CNPJ ALFANUMERICO                            *
      *================================================================*
      *
       01  GFCTZJ-REGISTRO.
           03  GFCTZJ-COD-AGENCIA       PIC  9(005)    COMP-3.
           03  GFCTZJ-NUM-CONTA         PIC  9(013)    COMP-3.
           03  GFCTZJ-COD-GRUPO         PIC  9(003)    COMP-3.
           03  GFCTZJ-COD-SUB-GRUPO     PIC  9(003)    COMP-3.
           03  GFCTZJ-CNPJ-CPF          PIC  9(015).
           03  GFCTZJ-CNPJ-CPF-X        REDEFINES GFCTZJ-CNPJ-CPF.
ST2507*        05  GFCTZJ-CNPJ-CPF-PRINCIPAL
ST2507*                                 PIC  9(009).
ST2507         05  GFCTZJ-CNPJ-CPF-PRINCIPAL
ST2507                                  PIC  X(009).
ST2507*        05  GFCTZJ-CNPJ-CPF-FILIAL
ST2507*                                 PIC  9(004).
ST2507         05  GFCTZJ-CNPJ-CPF-FILIAL
ST2507                                  PIC  X(004).
               05  GFCTZJ-CNPJ-CPF-CONTROLE
                                        PIC  9(002).
           03  GFCTZJ-COD-EVENTO-TARIFA PIC  9(005)    COMP-3.
           03  GFCTZJ-DATA-DEBITO       PIC  9(008).
           03  GFCTZJ-DATA-DEBITO-X     REDEFINES GFCTZJ-DATA-DEBITO.
               05  GFCTZJ-ANO-DEBITO    PIC  9(004).
               05  GFCTZJ-MES-DEBITO    PIC  9(002).
               05  GFCTZJ-DIA-DEBITO    PIC  9(002).
           03  GFCTZJ-COD-IDENT-COBRANCA
                                        PIC  9(001).
           03  GFCTZJ-VLR-COBRADO       PIC  9(015)V99 COMP-3.
           03  GFCTZJ-COD-IDENT-ORIG-DEB
                                        PIC  9(001).
           03  GFCTZJ-DATA-EVENTO       PIC  9(008).
           03  GFCTZJ-DATA-EVENTO-X     REDEFINES GFCTZJ-DATA-EVENTO.
               05  GFCTZJ-ANO-EVENTO    PIC  9(004).
               05  GFCTZJ-MES-EVENTO    PIC  9(002).
               05  GFCTZJ-DIA-EVENTO    PIC  9(002).
           03  FILLER                   PIC  X(021).
