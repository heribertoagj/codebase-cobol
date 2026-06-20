      *
      ******************************************************************
      * NOME DA INC - GFCTWE00                                         *
      * DESCRICAO   - ARQUIVO DE COMUNICACAO GFCT E TRPN (INPUT)       *
      *               RELACAO DE CLIENTES DO TRPN                      *
      * TAMANHO     - 078                                              *
      * DATA        - NOV/2014                                         *
      * RESPONSAVEL - PAGNOCCA - SONDA IT                              *
      *================================================================*
ST2507*               U L T I M A   A L T E R A C A O                *
      *----------------------------------------------------------------*
ST2507*    ANALISTA....:  STEFANINI                                    *
ST2507*    DATA........:  07 / 2025                                    *
ST2507*    OBJETIVO....:  CNPJ ALFANUMERICO                            *
      *================================================================*
      *
       01  GFCTWE00-REG.
           05 GFCTWE00-INPUT.
              10 WE00-COD-CLIENTE          PIC  9(009).
ST2507*       10 WE00-CPF-CNPJ-PRINC       PIC  9(009).
ST2507*       10 WE00-CPF-CNPJ-FILIAL      PIC  9(005).
ST2507        10 WE00-CPF-CNPJ-PRINC       PIC  X(009).
ST2507        10 WE00-CPF-CNPJ-FILIAL      PIC  X(004).
              10 WE00-CPF-CNPJ-CONTR       PIC  9(003).
              10 WE00-CAG-BCRIA            PIC  9(005).
              10 WE00-CCTA-BCRIA           PIC  9(013).
              10 WE00-CSERVC-TARIF-TRPN    PIC  9(009).
              10 WE00-CSERVC-TARIF-GFCT    PIC  9(005).
ST2507*    05 WE00-RESERVA                 PIC  X(020).
ST2507     05 WE00-RESERVA                 PIC  X(021).
      *
      ************** FIM DA GFCTWE00 ***********************************

