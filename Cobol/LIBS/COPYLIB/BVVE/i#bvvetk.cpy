      *================================================================*
      * I#BVVETK - TRAVA AUTOMATICA                                    *
      *            CADASTRO CIELO EXPANDIDO                            *
      *----------------------------------------------------------------*
      * LRECL: 350                                                     *
      *================================================================*
       01  BVVETK-REGISTRO.
           05  BVVETK-TIPO-REG             PIC  9(001).
      *        0 = HEADER
      *        1 = DETALHE
      *        2 = VOUCHER
      *        9 = TRAILLER

           05  BVVETK-HEADER.
               10  BVVETK-HD-PERIODO       PIC  9(006).
      *            PERIODO = AAAAMM
               10  BVVETK-HD-SEQ-ENVIO     PIC  9(006).
               10  BVVETK-HD-RESERVA       PIC  X(337).

           05  BVVETK-DETALHE      REDEFINES  BVVETK-HEADER.
               10  BVVETK-ESTABELEC        PIC  9(010).
               10  BVVETK-BANCO            PIC  9(004).
               10  BVVETK-AGENCIA          PIC  9(005).
               10  BVVETK-CTA-AUX1         PIC  X(002).
               10  BVVETK-CONTA            PIC  9(007).
               10  BVVETK-CONTA-DIG        PIC  X(001).
               10  BVVETK-CTA-AUX2         PIC  X(004).
               10  BVVETK-NOME-ESTAB       PIC  X(032).
               10  BVVETK-CNPJ-CPF         PIC  9(017).
               10  BVVETK-DT-ABERTURA      PIC  9(008).
      *            DATA ABERTURA = AAAAMMDD
               10  BVVETK-ENDERECO         PIC  X(032).
               10  BVVETK-COMPLEMENTO      PIC  X(032).
               10  BVVETK-CIDADE           PIC  X(028).
               10  BVVETK-UF               PIC  X(002).
               10  BVVETK-CEP              PIC  9(009).
               10  BVVETK-RAMO-ATIV        PIC  9(005).
               10  BVVETK-STATUS           PIC  X(001).
      *            O = OPEN
      *            F = FROZEN
      *            C = CLOSE
      *            R = REOPENED
      *            T = TRANFER
      *            D = DESACTIVATED
      *            P = NEW ACCOUNT PENDING
               10  BVVETK-CATEG-ANTEC      PIC  X(001).
               10  BVVETK-FLAG-POS         PIC  X(001).
      *            'S' OU 'N'
               10  BVVETK-NOME-CONTATO     PIC  X(032).
               10  BVVETK-DDD-TELEFONE     PIC  9(007).
               10  BVVETK-NRO-TELEFONE     PIC  9(011).
               10  BVVETK-TAXA             PIC  9(003)V9999.
               10  BVVETK-PRAZO            PIC  9(002).
               10  BVVETK-DT-PROCESS       PIC  9(008).
      *            DATA PROCESSAMENTO = AAAAMMDD
               10  BVVETK-SN-VR-BRUTO      PIC  X(001).
               10  BVVETK-VR-BRUTO         PIC  9(013)V99.
               10  BVVETK-SN-VR-LIQUIDO    PIC  X(001).
               10  BVVETK-VR-LIQUIDO       PIC  9(013)V99.
               10  BVVETK-TP-ESTAB         PIC  X(001).
      *            M = MATRIZ
      *            F = FILIAL
      *            I = INDIVIDUAL
               10  BVVETK-TP-CADEIA        PIC  X(001).
      *            'C' = CENTRALIZADO
      *            'D' = DESCENTRALIZADO
      *            ' ' = INDEPENDENTE
               10  BVVETK-ESTAB-MATRIZ     PIC  9(010).
      *            ZEROS PARA INDIVIDUAL
               10  BVVETK-SN-VR-BRUTO-41   PIC  X(001).
               10  BVVETK-VR-BRUTO-41      PIC  9(013)V99.
               10  BVVETK-SN-VR-LIQUID-41  PIC  X(001).
               10  BVVETK-VR-LIQUIDO-41    PIC  9(013)V99.
               10  BVVETK-STATUS-ATIV      PIC  X(001).
      *            'I' = MAIS DE 180 DIAS SEM VENDAS
      *            'F' = MAIS DE 30 DIAS SEM VENDAS
      *            'A' = ESTABELECIMENTO ATIVO
      *            ' ' = INATIVO A MAIS DE 360 DIAS
               10  BVVETK-PERIODO-ATIV     PIC  9(002).
               10  BVVETK-COMPL-MCC        PIC  X(001).
               10  BVVETK-BANDEIRA         PIC  X(001).
      *            '1' = VISA
      *            '2' = MASTER
      *            '3' = AGIPLAN
      *            '4' = BANESCARD
      *            '5' = CABAL
      *            '6' = SOROCRED
      *            '7' = ELO
      *            '8' = CREDSYSTEM
      *            '9' = DINERS/DISCOVER
      *            'B' = REDESPLAN
      *            'C' = CREDZ

           05  BVVETK-TRAILLER     REDEFINES  BVVETK-HEADER.
               10  BVVETK-TR-QTDE-ESTAB    PIC  9(009).
               10  BVVETK-TR-SN-BRUTO      PIC  X(001).
               10  BVVETK-TR-VR-BRUTO      PIC  9(013)V99.
               10  BVVETK-TR-SN-LIQUIDO    PIC  X(001).
               10  BVVETK-TR-VR-LIQUIDO    PIC  9(013)V99.
               10  BVVETK-TR-RESERVA       PIC  X(308).
