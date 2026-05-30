      *================================================================*
      * DESCRICAO  : LISTA E MANUTENCAO DE TESTEMUNHAS DE UMA OPERACAO *
      *              /PROPOSTA DE DESCONTO COMERCIAL - LAYOUT SAIDA    *
      * COPYBOOK   : DCOMWA1S - BOOK DE ACESSO AO PROGRAMA FUNCIONAL   *
      * COORDENADOR: DCOM1ALL - LISTA E MANUTENCAO DE TESTEMUNHAS      *
      * FLUXO      : DCOMIABB                                         *
      * DATA       : 18/06/2024                                        *
      * AUTOR      : LUCIANDRA SILVEIRA - WIPRO                        *
      * COMPONENTE : DCOM - DESCONTO COMERCIAL                         *
      *----------------------------------------------------------------*
      * DATA       AUTOR             DESCRICAO / MANUTENCAO            *
      *----------------------------------------------------------------*
      * XX/XX/XXXX XXXXXXXXXXXXXXXXX XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX  ***
      *================================================================*
      *
          05 DCOMWA1S-HEADER.
             10 DCOMWA1S-COD-LAYOUT    PIC X(008)   VALUE 'DCOMWA1S'.
             10 DCOMWA1S-TAM-LAYOUT    PIC 9(005)   VALUE 12665.

             10 DCOMWA1S-BLOCO-SAIDA.
                15 DCOMWA1S-QTDE-TESTEMUNHAS       PIC  9(003).
                15 DCOMWA1S-TESTEMUNHAS            OCCURS 100 TIMES.
                   20 DCOMWA1S-CBCO                PIC  9(003).
                   20 DCOMWA1S-CAG-BCRIA           PIC  9(005).
                   20 DCOMWA1S-NPSSOA-DESC-COML    PIC  9(009).
                   20 DCOMWA1S-CCPF                PIC  9(009).
                   20 DCOMWA1S-CCTRL-CPF           PIC  9(002).
                   20 DCOMWA1S-IPSSOA-DESC-COML    PIC  X(060).
                   20 DCOMWA1S-CCLUB               PIC  9(010).
                   20 DCOMWA1S-CDOCTO-ID           PIC  X(015).
                   20 DCOMWA1S-TEM-NA-AGENCIA      PIC  X(001).
      *-->      ----------------------------------------------------
      *-->  -->    AREA RESERVA PARA EXPANSAO DE DADOS               <--
      *-->      ----------------------------------------------------
                15 DCOMWA1I-S-RESERVA             PIC  X(1249).
      *
