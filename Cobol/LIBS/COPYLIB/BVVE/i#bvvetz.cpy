      *================================================================*
      * I#BVVETZ - TRAVA AUTOMATICA                                    *
      *            CADASTRO CIELO EXPANDIDO                            *
      *----------------------------------------------------------------*
      * LRECL: 370                                                     *
      *================================================================*
       01  BVVETZ-REGISTRO.
           05  BVVETZ-TIPO-REG             PIC  9(001).
      *        0 = HEADER
      *        1 = DETALHE
      *        2 = VOUCHER
      *        9 = TRAILLER

           05  BVVETZ-HEADER.
               10  BVVETZ-HD-PERIODO       PIC  9(006).
      *            PERIODO = AAAAMM
               10  BVVETZ-HD-SEQ-ENVIO     PIC  9(006).
               10  BVVETZ-HD-RESERVA       PIC  X(358).

           05  BVVETZ-DETALHE      REDEFINES  BVVETZ-HEADER.
               10  BVVETZ-ESTABELEC        PIC  9(010).
               10  BVVETZ-BANCO            PIC  9(004).
               10  BVVETZ-AGENCIA          PIC  9(005).
               10  BVVETZ-CTA-AUX1         PIC  X(002).
               10  BVVETZ-CONTA            PIC  9(007).
               10  BVVETZ-CONTA-DIG        PIC  X(001).
               10  BVVETZ-CTA-AUX2         PIC  X(004).
               10  BVVETZ-NOME-ESTAB       PIC  X(032).
               10  BVVETZ-CNPJ-CPF         PIC  9(017).
               10  BVVETZ-DT-ABERTURA      PIC  9(008).
      *            DATA ABERTURA = AAAAMMDD
               10  BVVETZ-ENDERECO         PIC  X(032).
               10  BVVETZ-COMPLEMENTO      PIC  X(032).
               10  BVVETZ-CIDADE           PIC  X(028).
               10  BVVETZ-UF               PIC  X(002).
               10  BVVETZ-CEP              PIC  9(009).
               10  BVVETZ-RAMO-ATIV        PIC  9(005).
               10  BVVETZ-STATUS           PIC  X(001).
      *            O = OPEN
      *            F = FROZEN
      *            C = CLOSE
      *            R = REOPENED
      *            T = TRANFER
      *            D = DESACTIVATED
      *            P = NEW ACCOUNT PENDING
               10  BVVETZ-CATEG-ANTEC      PIC  X(001).
               10  BVVETZ-FLAG-POS         PIC  X(001).
      *            'S' OU 'N'
               10  BVVETZ-NOME-CONTATO     PIC  X(032).
               10  BVVETZ-DDD-TELEFONE     PIC  9(007).
               10  BVVETZ-NRO-TELEFONE     PIC  9(011).
               10  BVVETZ-TAXA             PIC  9(003)V9999.
               10  BVVETZ-PRAZO            PIC  9(002).
               10  BVVETZ-DT-PROCESS       PIC  9(008).
      *            DATA PROCESSAMENTO = AAAAMMDD
               10  BVVETZ-SN-VR-BRUTO      PIC  X(001).
               10  BVVETZ-VR-BRUTO         PIC  9(013)V99.
               10  BVVETZ-SN-VR-LIQUIDO    PIC  X(001).
               10  BVVETZ-VR-LIQUIDO       PIC  9(013)V99.
               10  BVVETZ-TP-ESTAB         PIC  X(001).
      *            M = MATRIZ
      *            F = FILIAL
      *            I = INDIVIDUAL
               10  BVVETZ-TP-CADEIA        PIC  X(001).
      *            'C' = CENTRALIZADO
      *            'D' = DESCENTRALIZADO
      *            ' ' = INDEPENDENTE
               10  BVVETZ-ESTAB-MATRIZ     PIC  9(010).
      *            ZEROS PARA INDIVIDUAL
               10  BVVETZ-SN-VR-BRUTO-41   PIC  X(001).
               10  BVVETZ-VR-BRUTO-41      PIC  9(013)V99.
               10  BVVETZ-SN-VR-LIQUID-41  PIC  X(001).
               10  BVVETZ-VR-LIQUIDO-41    PIC  9(013)V99.
               10  BVVETZ-STATUS-ATIV      PIC  X(001).
      *            'I' = MAIS DE 180 DIAS SEM VENDAS
      *            'F' = MAIS DE 30 DIAS SEM VENDAS
      *            'A' = ESTABELECIMENTO ATIVO
      *            ' ' = INATIVO A MAIS DE 360 DIAS
               10  BVVETZ-PERIODO-ATIV     PIC  9(002).
               10  BVVETZ-COMPL-MCC        PIC  X(001).
               10  BVVETZ-BANDEIRA         PIC  X(001).
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
               10  BVVETZ-BANDEIRA-SCG     PIC  X(003).
               10  BVVETZ-DT-RESERVA       PIC  X(017).

           05  BVVETZ-TRAILLER     REDEFINES  BVVETZ-HEADER.
               10  BVVETZ-TR-QTDE-ESTAB    PIC  9(009).
               10  BVVETZ-TR-SN-BRUTO      PIC  X(001).
               10  BVVETZ-TR-VR-BRUTO      PIC  9(013)V99.
               10  BVVETZ-TR-SN-LIQUIDO    PIC  X(001).
               10  BVVETZ-TR-VR-LIQUIDO    PIC  9(013)V99.
               10  BVVETZ-TR-RESERVA       PIC  X(329).
