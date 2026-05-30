      *================================================================*
      * I#BVVELI - TRAVA AUTOMATICA                                    *
      *            CADASTRO CIELO EXPANDIDO                            *
      *----------------------------------------------------------------*
      *   CLONE DO BOOK I#BVVETK  COM ADICAO DO DE/PARA DE BANDEIRA    *
      *----------------------------------------------------------------*
      * LRECL: 353                                                     *
      *================================================================*
       01  BVVELI-REGISTRO.
           05  BVVELI-TIPO-REG             PIC  9(001).
      *        0 = HEADER
      *        1 = DETALHE
      *        2 = VOUCHER
      *        9 = TRAILLER

           05  BVVELI-HEADER.
               10  BVVELI-HD-PERIODO       PIC  9(006).
      *            PERIODO = AAAAMM
               10  BVVELI-HD-SEQ-ENVIO     PIC  9(006).
LAR            10  BVVELI-HD-RESERVA       PIC  X(340).

           05  BVVELI-DETALHE      REDEFINES  BVVELI-HEADER.
               10  BVVELI-ESTABELEC        PIC  9(010).
               10  BVVELI-BANCO            PIC  9(004).
               10  BVVELI-AGENCIA          PIC  9(005).
               10  BVVELI-CTA-AUX1         PIC  X(002).
               10  BVVELI-CONTA            PIC  9(007).
               10  BVVELI-CONTA-DIG        PIC  X(001).
               10  BVVELI-CTA-AUX2         PIC  X(004).
               10  BVVELI-NOME-ESTAB       PIC  X(032).
               10  BVVELI-CNPJ-CPF         PIC  9(017).
               10  BVVELI-DT-ABERTURA      PIC  9(008).
      *            DATA ABERTURA = AAAAMMDD
               10  BVVELI-ENDERECO         PIC  X(032).
               10  BVVELI-COMPLEMENTO      PIC  X(032).
               10  BVVELI-CIDADE           PIC  X(028).
               10  BVVELI-UF               PIC  X(002).
               10  BVVELI-CEP              PIC  9(009).
               10  BVVELI-RAMO-ATIV        PIC  9(005).
               10  BVVELI-STATUS           PIC  X(001).
      *            O = OPEN
      *            F = FROZEN
      *            C = CLOSE
      *            R = REOPENED
      *            T = TRANFER
      *            D = DESACTIVATED
      *            P = NEW ACCOUNT PENDING
               10  BVVELI-CATEG-ANTEC      PIC  X(001).
               10  BVVELI-FLAG-POS         PIC  X(001).
      *            'S' OU 'N'
               10  BVVELI-NOME-CONTATO     PIC  X(032).
               10  BVVELI-DDD-TELEFONE     PIC  9(007).
               10  BVVELI-NRO-TELEFONE     PIC  9(011).
               10  BVVELI-TAXA             PIC  9(003)V9999.
               10  BVVELI-PRAZO            PIC  9(002).
               10  BVVELI-DT-PROCESS       PIC  9(008).
      *            DATA PROCESSAMENTO = AAAAMMDD
               10  BVVELI-SN-VR-BRUTO      PIC  X(001).
               10  BVVELI-VR-BRUTO         PIC  9(013)V99.
               10  BVVELI-SN-VR-LIQUIDO    PIC  X(001).
               10  BVVELI-VR-LIQUIDO       PIC  9(013)V99.
               10  BVVELI-TP-ESTAB         PIC  X(001).
      *            M = MATRIZ
      *            F = FILIAL
      *            I = INDIVIDUAL
               10  BVVELI-TP-CADEIA        PIC  X(001).
      *            'C' = CENTRALIZADO
      *            'D' = DESCENTRALIZADO
      *            ' ' = INDEPENDENTE
               10  BVVELI-ESTAB-MATRIZ     PIC  9(010).
      *            ZEROS PARA INDIVIDUAL
               10  BVVELI-SN-VR-BRUTO-41   PIC  X(001).
               10  BVVELI-VR-BRUTO-41      PIC  9(013)V99.
               10  BVVELI-SN-VR-LIQUID-41  PIC  X(001).
               10  BVVELI-VR-LIQUIDO-41    PIC  9(013)V99.
               10  BVVELI-STATUS-ATIV      PIC  X(001).
      *            'I' = MAIS DE 180 DIAS SEM VENDAS
      *            'F' = MAIS DE 30 DIAS SEM VENDAS
      *            'A' = ESTABELECIMENTO ATIVO
      *            ' ' = INATIVO A MAIS DE 360 DIAS
               10  BVVELI-PERIODO-ATIV     PIC  9(002).
               10  BVVELI-COMPL-MCC        PIC  X(001).
               10  BVVELI-BANDEIRA         PIC  X(001).
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
LAR            10  BVVELI-BANDEIRA-DP      PIC  9(003).

           05  BVVELI-TRAILLER     REDEFINES  BVVELI-HEADER.
               10  BVVELI-TR-QTDE-ESTAB    PIC  9(009).
               10  BVVELI-TR-SN-BRUTO      PIC  X(001).
               10  BVVELI-TR-VR-BRUTO      PIC  9(013)V99.
               10  BVVELI-TR-SN-LIQUIDO    PIC  X(001).
               10  BVVELI-TR-VR-LIQUIDO    PIC  9(013)V99.
LAR            10  BVVELI-TR-RESERVA       PIC  X(311).
