      ******************************************************************
      *  RESOLUCAO 4734 - CLIENTES MASSIVOS                            *
      *----------------------------------------------------------------*
      *  COMPONENTE...: BVVEW521 - AREA DE ACESSO AO BVVEA521          *
      *  DESCRICAO....: IDENTIFICAR CLIENTES MASSIVOS                  *
      *                   (BASEADO NO BOOK I#BVVEFX)                   *
      *  IND-CLI-MASSV: 'S' SIM OU 'N' NAO                             *
      ******************************************************************
      *                                                                *
       05 BVVEW521-HEADER.
          10 BVVEW521-COD-LAYOUT    PIC X(008) VALUE 'BVVEW521'.
          10 BVVEW521-TAM-LAYOUT    PIC 9(005) VALUE 00084.
      *
       05 BVVEW521-REGISTRO.
          10 BVVEW521-BLOCO-ENTRADA.
             15 BVVEW521-E-AGENCIA             PIC  9(005).
             15 BVVEW521-E-CONTA               PIC  9(013).
      *
          10 BVVEW521-BLOCO-SAIDA.
             15 BVVEW521-S-COD-RETORNO         PIC  9(002).
             15 BVVEW521-S-MSG-RETORNO         PIC  X(050).
             15 BVVEW521-S-IND-CLI-MASSV       PIC  X(001).
