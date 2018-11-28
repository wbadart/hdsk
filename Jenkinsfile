pipeline {
  agent any
  stages {
    stage('Pre-build') {
      steps {
        sh '''mkdir -p ~/.local/bin
curl -L https://get.haskellstack.org/stable/linux-x86_64.tar.gz | tar xz --wildcards --strip-components=1 -C ~/.local/bin \'*/stack\''''
      }
    }
  }
}