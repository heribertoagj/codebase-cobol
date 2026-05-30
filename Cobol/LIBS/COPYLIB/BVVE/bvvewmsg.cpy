      *----------------------------------------------------------------*
      *                   COPYBOOK - CONSTANTES GMSG                   *
      *----------------------------------------------------------------*
      *
       01  BVVEWMSG.
           05  BVVEWMSG-IDIOMA                         PIC 9(03).
               88  BVVEWMSG-IDI-DEFAULT                VALUE      1.
               88  BVVEWMSG-IDI-PORTUGUES              VALUE      1.
               88  BVVEWMSG-IDI-INGLES                 VALUE      2.
               88  BVVEWMSG-IDI-ESPANHOL               VALUE      3.
               88  BVVEWMSG-IDI-FRANCES                VALUE      4.
               88  BVVEWMSG-IDI-005                    VALUE      5.
      *
           05  BVVEWMSG-TIPOS-RECURSO                  PIC 9(03).
               88  BVVEWMSG-TREC-CANAL                 VALUE      0.
               88  BVVEWMSG-TREC-GLOG                  VALUE      1.
               88  BVVEWMSG-TREC-SMS                   VALUE      2.
      *
           05  BVVEWMSG-RECURSO                        PIC 9(05).
               88  BVVEWMSG-REC-CANAL-DEFAULT          VALUE      0.
               88  BVVEWMSG-REC-TERMINAL-GERENCIA      VALUE     10.
               88  BVVEWMSG-REC-GLOG                   VALUE     11.
               88  BVVEWMSG-REC-BDN                    VALUE     20.
               88  BVVEWMSG-REC-SMS                    VALUE     21.
               88  BVVEWMSG-REC-AGENCIAS               VALUE     50.
               88  BVVEWMSG-REC-INTERNET-BANKING       VALUE    140.
               88  BVVEWMSG-REC-NET-EMPRESA            VALUE    350.
               88  BVVEWMSG-REC-INTRACORP-BRADESC      VALUE    500.
               88  BVVEWMSG-REC-00630                  VALUE    630.
               88  BVVEWMSG-REC-00650                  VALUE    650.
               88  BVVEWMSG-REC-BRADESCO-CELULAR       VALUE    660.
               88  BVVEWMSG-REC-05090                  VALUE   5090.
               88  BVVEWMSG-REC-05130                  VALUE   5130.
               88  BVVEWMSG-REC-05210                  VALUE   5210.
               88  BVVEWMSG-REC-05340                  VALUE   5340.
               88  BVVEWMSG-REC-05370                  VALUE   5370.
               88  BVVEWMSG-REC-05380                  VALUE   5380.
               88  BVVEWMSG-REC-05410                  VALUE   5410.
      *
           05  BVVEWMSG-COD-MENSAGEM                   PIC X(08).
               88  BVVE-ERRO-ACESSO-FNCIONAL           VALUE 'BVVE0001'.
               88  BVVE-ERRO-ACESSO-FRWK1200           VALUE 'BVVE0002'.
               88  BVVE-ERRO-ACESSO-MODULO             VALUE 'BVVE0003'.
               88  BVVE-ERRO-ACESSO-DB2                VALUE 'BVVE0004'.
               88  BVVE-CONSULTA-SUCESSO               VALUE 'BVVE0005'.
               88  BVVE-NAO-EXISTEM-REG                VALUE 'BVVE0006'.
               88  BVVE-IND-PAG-INVALIDO               VALUE 'BVVE0007'.
               88  BVVE-CPF-CNPJ-INVALIDO              VALUE 'BVVE0008'.
               88  BVVE-USUARIO-INVALIDO               VALUE 'BVVE0009'.
               88  BVVE-INCLUSÃO-COM-SUCESSO           VALUE 'BVVE0010'.
               88  BVVE-ALTERAÇÃO-SUCESSO              VALUE 'BVVE0011'.
               88  BVVE-OPERACAO-SUCESSO               VALUE 'BVVE0012'.
               88  BVVE-DADOS-PESQ-INVALIDOS           VALUE 'BVVE0013'.
               88  BVVE-MANUTENCAO-SUCESSO             VALUE 'BVVE0014'.
               88  BVVE-OPTIN-AGENDA                   VALUE 'BVVE0015'.
      *
      *----------------------------------------------------------------*
