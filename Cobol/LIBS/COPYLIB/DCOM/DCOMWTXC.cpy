      ******************************************************************
      * DESCRICAO  : CONVERTER TAXA NA FREQUENCIA DIA, MES E ANO CON-  *
      *              FORME A TAXA E FREQUENCIA INFORMADA               *
      * COPYBOOK   : DCOMWTXC - BOOK DE ACESSO AO PROGRAMA BASE        *
      * FUNCIONAL  : DCOM3TXC - CONVERTER TAXA PARA AS FREQUENCIAS DIA-*
      *              RIA, MENSAL E ANUAL                               *
      * AUTOR      : LUCIANDRA SILVEIRA - WIPRO                        *
      * COMPONENTE : DCOM - DESCONTO COMERCIAL                         *
      *----------------------------------------------------------------*
      * DATA       AUTOR             DESCRICAO / MANUTENCAO            *
      *----------------------------------------------------------------*
      * XX/XX/XXXX XXXXXXXXXXXXXXXXX XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX  ***
      ******************************************************************
          05 DCOMWTXC-HEADER.
             10 DCOMWTXC-COD-LAYOUT    PIC X(008) VALUE 'DCOMWTXC'.
             10 DCOMWTXC-TAM-LAYOUT    PIC 9(005) VALUE 00200.
      *
          05 DCOMWTXC-REGISTRO.
             10 DCOMWTXC-BLOCO-ENTRADA.
                15 DCOMWTXC-E-FUNCAO              PIC  9(001).
                15 DCOMWTXC-E-CCANAL              PIC  9(003).
                15 DCOMWTXC-E-CPRODT              PIC  9(003).
                15 DCOMWTXC-E-CSPROD-DESC-COML    PIC  9(003).
                15 DCOMWTXC-E-CINDCD-CALC-JURO    PIC  X(001).
                15 DCOMWTXC-E-CINDCD-FREQ-TX      PIC  X(002).
                15 DCOMWTXC-E-PTX                 PIC S9(003)V9(006).
                15 DCOMWTXC-E-PRZ-MEDIO           PIC  9(005).
                15 DCOMWTXC-E-PRZ-PCELA           PIC  9(005).
                15 DCOMWTXC-E-VLR-PCELA           PIC S9(015)V9(002).
                15 DCOMWTXC-E-RESERVA             PIC  X(025).
      *
             10 DCOMWTXC-BLOCO-SAIDA.
                15 DCOMWTXC-S-PTX-MENSAL          PIC S9(003)V9(006).
                15 DCOMWTXC-S-PTX-ANUAL           PIC S9(003)V9(006).
                15 DCOMWTXC-S-PTX-DIARIA          PIC S9(003)V9(006).
                15 DCOMWTXC-S-PTX-CUSTO-MES       PIC S9(003)V9(006).
                15 DCOMWTXC-S-PTX-CUSTO-DIA       PIC S9(003)V9(006).
                15 DCOMWTXC-S-PTX-CUSTO-ANO       PIC S9(003)V9(006).
                15 DCOMWTXC-S-VLR-DESPESA         PIC S9(015)V9(002).
                15 DCOMWTXC-S-RESERVA             PIC  X(042).
      *
